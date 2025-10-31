// SPDX-License-Identifier: Apache-2.0
//
// SPDX-FileCopyrightText: 2025 Cedric Hammes <contact@cach30verfl0w.net>

//! This crate is a no-std compatible and zero-copy parser for the **devicetree binary format (DTB)** in Rust. When using this crate, you
//! can disable the default features to use this in embedded environments.
//!
//! ## What is a device tree?
//! Devicetree is a data structure describing hardware components of a particular computer to give a kernel/an OS the information required
//! to manage those components including the CPU and memory. On many ARM computers, for example, it is possible to read a device tree, while
//! UEFI and x86-based environments rely on protocols such as ACPI.
//! ```text
//!                                                            cpu@0
//!                                                    ┌────────────────────────┐
//!                                                    │device_type="cpu"       │
//!                                                    │reg=<0x0>               │
//!                                             ┌─────►│timebase-frequency=<1>  │
//!                                             │      │clock-frequency=<1>     │
//!                                             │      └────────────────────────┘
//!                                             │
//!          /                      cpu         │             cpu@1
//! ┌───────────────────┐    ┌──────────────────┴─┐    ┌────────────────────────┐
//! │compatbile="test"  │    │#address-cells=<1>  ├───►│device_type="cpu"       │
//! │model="test"       ├───►│#size-cells=<1>     │    │reg=<0x0>               │
//! │#address-cells=<1> │    └────────────────────┘    │timebase-frequency=<1>  │
//! │#size-cells=<1>    │                              │clock-frequency=<1>     │
//! └─────────────────┬─┘           memory@0           └────────────────────────┘
//!                   │      ┌────────────────────┐
//!                   │      │device_type="memory"│
//!                   └─────►│reg=<0 0x200>       │
//!                          └────────────────────┘
//!
//!
//! ```
//!
//! ## Features
//! - **no-std:** This crate can be used in embedded environments without a Rust standard library
//! - **zero-copy:** All structures serialized are not being copied when creating from slice or pointer
//!
//! For using the **no-std** feature of the crate, you should disable the default features when defining this crate in your `Cargo.toml`:
//! ```toml
//! devicetree-rs = { version = "x.y.z", default-features = false }
//! ```
//!
//! ## Examples
//! ```rust
//! use devicetree::BinaryDeviceTree;
//! use std::num::NonZeroU64;
//!
//! fn main() {
//!     let devicetree = BinaryDeviceTree::from_ptr(NonZeroU64::new(0x2000_0000).unwrap()).unwrap();
//!     let soc_node = devicetree.root_node().find_child("soc").unwrap();
//!     // TODO: Implement example of reading the address mapping and map the address of the
//! }
//! ```
//!
//! ## References
//! - [Devicetree Specification, Release v0.4](https://github.com/devicetree-org/devicetree-specification/releases/tag/v0.4)

#![cfg_attr(not(feature = "std"), no_std)]

pub(crate) mod stack;
pub(crate) mod cow;

#[cfg(test)]
#[cfg(feature = "std")]
mod tests;
pub mod structures;

#[cfg(all(feature = "alloc", not(feature = "std")))]
extern crate alloc;

#[cfg(not(feature = "std"))]
pub(crate) use core as std;

#[cfg(feature = "std")]
pub(crate) use std as std;

#[cfg(feature = "std")]
pub(crate) use std as alloc;

use std::{
    fmt::{
        Display,
        Formatter
    },
    fmt,
    error::Error as ErrorExt,
    num::NonZeroU64,
    ptr::NonNull,
    slice,
    ffi::CStr
};
use nom::{
    IResult,
    combinator::map_res,
    number::complete::be_u32,
    Parser,
    sequence::terminated,
    bytes::{take_till, complete::{tag, take}},
    error::{ErrorKind, FromExternalError, ParseError},
};
use crate::{cow::Cow, stack::Stack};
use crate::structures::property::StructureBlockProperty;

/// This enum provides an error type representing all errors possible when working with this library. It includes additional validation and
/// parsing errors.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Error {
    // Header Validation
    InvalidMagicBytes(u32),
    InvalidTotalSize(u32),
    ExceedingBlockBounds(&'static str, u32, u32, u32),
    IncompatibleVersion(u32, u32),

    // Structure Block Token Validation
    InvalidClassName,
    InvalidPropertyName,
    InvalidStringValue,
    InvalidIntegerValue,
    InvalidStringOffset(u32, usize),

    // Other
    StackOverflow,
    Parser(ErrorKind)
}

impl From<nom::Err<Error>> for Error {
    fn from(value: nom::Err<Error>) -> Self {
        match value {
            nom::Err::Incomplete(_) => todo!("Not implemented yet"),
            nom::Err::Error(error) | nom::Err::Failure(error) => error
        }
    }
}

impl From<Error> for nom::Err<Error> {
    fn from(value: Error) -> Self {
        nom::Err::Error(value)
    }
}


impl ErrorExt for Error {}

impl<T> FromExternalError<T, Error> for Error {
    fn from_external_error(_input: T, _kind: ErrorKind, error: Error) -> Self {
        error
    }
}

impl<T> ParseError<T> for Error {
    fn from_error_kind(_input: T, kind: ErrorKind) -> Self {
        Self::Parser(kind)
    }

    fn append(_input: T, _kind: ErrorKind, other: Self) -> Self {
        other
    }
}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // Header validation
            Self::InvalidMagicBytes(got) => write!(formatter, "Expected 0xD00DFEED, but got 0x{:02X}", got),
            Self::InvalidTotalSize(got) => write!(
                formatter,
                "FDT has size of {}, but should be bigger than {} bytes", got, size_of::<DtbHeader>()
            ),
            Self::ExceedingBlockBounds(kind, block_offset, block_size, total_size) => write!(
                formatter,
                "{kind} block (offset={block_offset} + size={block_size}) exceeds total size ({total_size})"
            ),
            Self::IncompatibleVersion(expected, got) => write!(formatter, "Version {got} is not compatbile with {expected}"),

            // Structure block token validation
            Self::InvalidClassName => write!(formatter, "Unable to parse name of 'begin node' token in structure block"),
            Self::InvalidPropertyName => write!(formatter, "Unable to parse name of 'property' token in structure block"),
            Self::InvalidStringValue => write!(formatter, "Unable to parse value of 'property' token in structure block holding a string"),
            Self::InvalidIntegerValue => write!(formatter, "Unable to parse value of 'property' token in structure block hlolding an int"),
            Self::InvalidStringOffset(string_off, strings_len) => write!(
                formatter,
                "String offset ({string_off}) exceeds strings block len {strings_len}"
            ),

            // Other
            Self::StackOverflow => write!(formatter, "Stack Overflow"),
            Self::Parser(error) => write!(formatter, "Parsing Error => {}", error.description())
        }
    }
}

/// This struct implements a header containing information about the following DTB-formatted devicetree. It is the first being parsed when
/// calling one of the main functions.
///
/// ## References
/// - [Header, Flattened Devicetree Format (FDT); The Devicetree Specification v0.4](https://github.com/devicetree-org/devicetree-specification/releases/tag/v0.4)
#[repr(C)]
#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash, Default)]
pub struct DtbHeader {
    pub magic_bytes: u32,
    pub total_size: u32,
    pub struct_block_offset: usize,
    pub strings_block_offset: usize,
    pub memory_reservation_block_offset: u32,
    pub version: u32,
    pub last_compatible_version: u32,
    pub boot_physical_cpuid: u32,
    pub strings_block_size: usize,
    pub struct_block_size: usize,
}

impl DtbHeader {
    const LAST_COMPATIBLE_VERSION: u32 = 17; // The current version

    fn from_be(data: &[u8]) -> IResult<&[u8], Self, Error> {
        map_res(
            (be_u32, be_u32, be_u32, be_u32, be_u32, be_u32, be_u32, be_u32, be_u32, be_u32),
            |(
                 magic_bytes,
                 total_size,
                 struct_block_offset,
                 strings_block_offset,
                 memory_reservation_block_offset,
                 version,
                 last_compatible_version,
                 boot_physical_cpuid,
                 strings_block_size,
                 struct_block_size,
             )| {
                if magic_bytes != 0xD00DFEED {
                    return Err(Error::InvalidMagicBytes(magic_bytes));
                }

                if total_size < size_of::<DtbHeader>() as _ {
                    return Err(Error::InvalidTotalSize(total_size));
                }

                if !(last_compatible_version..=version).contains(&Self::LAST_COMPATIBLE_VERSION) {
                    return Err(Error::IncompatibleVersion(Self::LAST_COMPATIBLE_VERSION, last_compatible_version));
                }

                if total_size < struct_block_offset + struct_block_size {
                    return Err(Error::ExceedingBlockBounds("Structure", struct_block_offset, struct_block_size, total_size));
                }

                if total_size < strings_block_offset + strings_block_size {
                    return Err(Error::ExceedingBlockBounds("Strings", strings_block_offset, strings_block_size, total_size));
                }

                Ok(Self {
                    magic_bytes,
                    total_size,
                    struct_block_offset: struct_block_offset as _,
                    strings_block_offset: strings_block_offset as _,
                    memory_reservation_block_offset,
                    version,
                    last_compatible_version,
                    boot_physical_cpuid,
                    strings_block_size: strings_block_size as _,
                    struct_block_size: struct_block_size as _,
                })
            },
        ).parse(data)
    }
}

/// ## References
/// - [Lexical Structure, Flattened Devicetree Format (FDT); The Devicetree Specification v0.4](https://github.com/devicetree-org/devicetree-specification/releases/tag/v0.4)
#[repr(C)]
#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
struct DtbPropertyHeader {
    length: u32,
    name_offset: u32
}

impl DtbPropertyHeader {
    fn from_be(data: &[u8]) -> IResult<&[u8], Self, Error> {
        map_res((be_u32, be_u32), |(length, name_offset)| {
            Ok::<Self, Error>(Self { length, name_offset })
        }).parse(data)
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub(crate) enum StructureBlockToken<'a> {
    BeginNode(&'a str),
    Property { name: &'a str, data: StructureBlockProperty<'a> },
    EndNode,
    Noop,
    SectionEnd
}

/// This struct is implementing a structure block token iterator which iterates all tokens in the Flattened Device Tree for extracting these
/// structures into a simpler view of nodes in a tree. If a node is invalid or the token list is exceeded, the next function returns none as
/// the value.
#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub(crate) struct StructureBlockTokenIterator<'a> {
    pub(crate) header: &'a DtbHeader,
    pub(crate) remaining_data: &'a [u8],
    pub(crate) strings_data: &'a [u8],
    pub(crate) node_stack: Stack<&'a str, 10> // TODO: We want to support stacks (allocated on the heap) when alloc crate is present
}

impl<'a> Iterator for StructureBlockTokenIterator<'a> {
    type Item = StructureBlockToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse().ok().and_then(|(remaining, item)| {
            if let Some(StructureBlockToken::SectionEnd) = item {
                // When we got a section end, we want to return a none. We don't update remaining data so if the next function is called
                // after this token, we don't read after the structure block section.
                return None;
            }

            self.remaining_data = remaining;
            item
        })
    }
}

impl<'a> StructureBlockTokenIterator<'a> {
    pub fn parse(&mut self) -> IResult<&'a [u8], Option<StructureBlockToken<'a>>, Error> {
        let start_length = self.remaining_data.len();
        let (remaining, node_type) = be_u32(self.remaining_data)?;
        let (remaining, token) = match node_type {
            0x00000001 => {
                let (remaining, name_bytes) = terminated(take_till(|b| b == 0), tag("\0")).parse(remaining)?;
                let name = str::from_utf8(name_bytes).map_err(|_| Error::InvalidClassName)?;
                self.node_stack.push(name)?;

                (remaining, Some(StructureBlockToken::BeginNode(name)))
            },
            0x00000003 => {
                let (remaining, header) = DtbPropertyHeader::from_be(remaining)?;
                let (remaining, data) = take(header.length).parse(remaining)?;
                if self.strings_data.len() < header.name_offset as _ {
                    return Err(nom::Err::Error(Error::InvalidStringOffset(header.name_offset, self.strings_data.len())));
                }

                let name = CStr::from_bytes_until_nul(&self.strings_data[header.name_offset as usize..])
                    .map_err(|_| Error::InvalidPropertyName)?
                    .to_str()
                    .map_err(|_| Error::InvalidPropertyName)?;

                (remaining, Some(StructureBlockToken::Property {
                    data: self.serialize_value(name, data)?,
                    name
                }))
            },
            0x00000002 => {
                self.node_stack.pop();
                (remaining, Some(StructureBlockToken::EndNode))
            },
            0x00000004 => (remaining, Some(StructureBlockToken::Noop)),
            0x00000009 => (remaining, Some(StructureBlockToken::SectionEnd)),
            _ => (remaining, None)
        };

        let consumed = start_length - remaining.len();
        Ok((&self.remaining_data[((consumed + 3) & !3)..], token))
    }

    fn serialize_value(&self, property_name: &str, data: &'a [u8]) -> Result<StructureBlockProperty<'a>, Error> {
        let node_name = *self.node_stack.top_value().unwrap(); // TODO: Handle error
        if node_name == "aliases" && property_name != "phandle" {
            return Ok(StructureBlockProperty::String(str::from_utf8(&data[0..data.len() - 1]).map_err(|_| Error::InvalidStringValue)?));
        }

        Ok(match property_name {
            "#address-cells" | "#size-cells" | "#interrupt-cells" | "phandle" | "virtual-reg" => {
                let integer = u32::from_be_bytes(data[0..data.len()].try_into().map_err(|_| Error::InvalidIntegerValue)?);
                StructureBlockProperty::UnsignedInt32(integer)
            },
            "compatible" | "model" | "status" | "device_type" | "clock-names" | "pinctrl-names" | "clock-output-names" | "dma-names"
            | "reg-names" | "phy-names" => {
                let string = str::from_utf8(&data[0..data.len() - 1]).map_err(|_| Error::InvalidStringValue)?;
                StructureBlockProperty::String(string)
            },
            _ => StructureBlockProperty::RawData(data)
        })
    }
}

fn read_cells(input: &[u8], cells: u32) -> IResult<&[u8], u64> {
    let mut rest = input;
    let mut acc = 0u64;

    for _ in 0..cells {
        let (new_rest, val) = be_u32(rest)?;
        acc = (acc << 32) | val as u64;
        rest = new_rest;
    }

    Ok((rest, acc))
}

#[derive(Clone)]
pub struct BusAddressSpacesMappingIterator<'a> {
    parent_address_cells: u32,
    child_address_cells: u32,
    address_range_size: u32,
    range_data: &'a [u8]
}

impl Iterator for BusAddressSpacesMappingIterator<'_> {
    type Item = (u64, u64, u64);

    fn next(&mut self) -> Option<Self::Item> {
        if self.range_data.len() < (self.parent_address_cells + self.child_address_cells + self.address_range_size) as _ {
            return None; // When the data is smaller than the size of one entry, we have to return None.
        }

        let (chunk, child_addr) = read_cells(self.range_data, self.child_address_cells).unwrap();
        let (chunk, parent_addr) = read_cells(chunk, self.parent_address_cells).unwrap();
        let (chunk, range_size) = read_cells(chunk, self.address_range_size).unwrap();
        self.range_data = chunk;
        Some((child_addr, parent_addr, range_size))
    }
}

pub struct MemoryRangesIterator<'a> {
    data: &'a [u8],
    addr_cells: u32,
    size_cells: u32,
}

impl Iterator for MemoryRangesIterator<'_> {
    type Item = (u64, u64);

    fn next(&mut self) -> Option<Self::Item> {
        if self.data.len() < (self.addr_cells + self.size_cells) as _ {
            return None;
        }

        let (chunk, addr) = read_cells(self.data, self.addr_cells).unwrap();
        let (chunk, size) = read_cells(chunk, self.size_cells).unwrap();
        self.data = chunk;
        Some((addr, size))
    }
}

/// This structure provides support for parsing a flattened device tree in memory by passing a pointer or slice or a file path for reading
/// from a file. It implements parsing based on the Devicetree Specification v0.4.
///
/// ## References
/// - [Flattened Devicetree (DTB) Format, Devicetree Specification v0.4](https://github.com/devicetree-org/devicetree-specification/releases/tag/v0.4)
#[derive(PartialEq, Debug)]
pub struct BinaryDeviceTree<'a> {
    pub header: DtbHeader,
    data: Cow<'a, [u8]>
}

impl<'a> BinaryDeviceTree<'a> {
    /// This function parses the flattened device tree by the specified base address. The address MUST point to the header without skipping
    /// the magic bytes. The data in the FDT must be big-endian as expected by the specification.
    ///
    /// ## Safety
    /// - The pointer must point to a valid binary device tree in the memory.
    pub unsafe fn from_ptr(base_address: NonZeroU64) -> Result<Self, Error> {
        let base_address = unsafe { NonNull::new_unchecked(base_address.get() as *mut _) };
        let (_, header) = DtbHeader::from_be(unsafe { slice::from_raw_parts(base_address.as_ptr(), size_of::<DtbHeader>()) })?;

        Ok(Self {
            data: Cow::Borrowed(unsafe { slice::from_raw_parts(base_address.as_ptr(), header.total_size as _) }),
            header
        })
    }

    /// This function parses the flattened device tree by the specified data slice. The slice MUST contain a valid binary devicetree with
    /// the header.
    pub fn from_slice(data: &'a [u8]) -> Result<Self, Error> {
        let header = DtbHeader::from_be(data)?.1;
        if header.total_size > data.len() as _ {
            return Err(Error::InvalidTotalSize(header.total_size));
        }

        Ok(Self { header, data: Cow::Borrowed(data) })
    }

    /// This function parses the flattened device tree by the specified file path. The path MUST point to a valid file containing valid DTB
    /// content.
    #[cfg(feature = "std")]
    pub fn from_path<T: AsRef<std::path::Path>>(path: T) -> Result<Self, Error> {
        let data = std::fs::read(path).unwrap();
        Ok(Self {
            header: DtbHeader::from_be(&data)?.1,
            data: Cow::Owned(data)
        })
    }

    pub fn memory_sections(&'a self) -> Option<MemoryRangesIterator<'a>> {
        let root_node = self.root_node();
        let memory_node = root_node.find_child("memory@0")?;
        Some(MemoryRangesIterator {
            data: memory_node.find_property("reg")?.as_bytes()?,
            size_cells: memory_node.find_property("#size-cells").or_else(|| root_node.find_property("#size-cells"))?.as_u32()?,
            addr_cells: memory_node.find_property("#address-cells").or_else(|| root_node.find_property("#address-cells"))?.as_u32()?
        })
    }

    /// This function returns an iterator returning a list of pairs (child_address, parent_address, range_size) where child_address is the
    /// address used for the devices, parent_address being the address being translated to and range_size the size of the address range. The
    /// range is calculated by defining a range from address to address + size.
    pub fn bus_address_spaces_mapping(&'a self) -> Option<BusAddressSpacesMappingIterator<'a>> {
        let root_node = self.root_node();
        let parent_address_cells = root_node.find_property("#address-cells")?.as_u32()?;

        let soc_node = root_node.find_child("soc")?;
        let child_address_cells = soc_node.find_property("#address-cells")?.as_u32()?;
        let address_range_size = soc_node.find_property("#size-cells")?.as_u32()?;
        let range_data = soc_node.find_property("ranges")?.as_bytes()?;

        Some(BusAddressSpacesMappingIterator {
            range_data,
            parent_address_cells,
            child_address_cells,
            address_range_size
        })
    }

    #[inline(always)]
    pub fn aliases(&'a self) -> Option<impl Iterator<Item = (&'a str, &'a str)>> {
        self.root_node().find_child("aliases").map(|node| node.properties().filter_map(|(name, value)| value.as_str().map(|x| (name, x))))
    }
}
