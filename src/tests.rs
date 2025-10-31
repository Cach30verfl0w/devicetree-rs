// SPDX-License-Identifier: GPL-2.0
//
// SPDX-FileCopyrightText: 2025 Cedric Hammes <contact@cach30verfl0w.net>

use std::slice;
use crate::{BinaryDeviceTree, DtbHeader, Error};

#[test]
fn node_enumeration() {
    let device_tree = BinaryDeviceTree::from_path("./test-files/bcm2711-raspberrypi-4b.dtb").unwrap();
    let Some(soc_node) = device_tree.root_node().find_child("soc") else {
        panic!("Unable to find SoC node");
    };

    assert_eq!(soc_node.children().collect::<Vec<_>>().len(), 67);
}

#[test]
fn bus_address_spaces_mapping() {
    let device_tree = BinaryDeviceTree::from_path("./test-files/bcm2711-raspberrypi-4b.dtb").unwrap();
    let actual_memory_mapping = device_tree.bus_address_spaces_mapping().unwrap().collect::<Vec<_>>();
    let expected_memory_mapping: [(u64, u64, u64); 3] = [
        (0x7E000000, 0xFE000000, 0x01800000),
        (0x7C000000, 0xFC000000, 0x02000000),
        (0x40000000, 0xFF800000, 0x00800000)
    ];

    assert_eq!(expected_memory_mapping, actual_memory_mapping.as_ref());
}

#[test]
fn find_node_by_alias() {
    let device_tree = BinaryDeviceTree::from_path("./test-files/bcm2711-raspberrypi-4b.dtb").unwrap();
    assert_eq!(device_tree.find_node_by_alias("uart0").map(|x| x.name), Some("serial@7e201000"));
}

#[test]
fn invalid_searches() {
    let device_tree = BinaryDeviceTree::from_path("./test-files/bcm2711-raspberrypi-4b.dtb").unwrap();
    assert!(device_tree.find_node_by_alias("invalid").is_none(), "Found invalid node with find_node_by_alias");
    assert!(device_tree.find_node_by_alias("uart0").and_then(|x| x.find_property("invalid")).is_none(), "Found invalid property");
    assert!(device_tree.root_node().find_child("test").is_none(), "Found invalid node with find_child");
}

#[test]
fn referencing_nodes_by_phandle() {
    let device_tree = BinaryDeviceTree::from_path("./test-files/bcm2711-raspberrypi-4b.dtb").unwrap();
    assert_eq!(device_tree.find_node_by_phandle(0x9).map(|x| x.name), Some("uart0_pins"));
}

#[test]
fn property_traversal() {
    let device_tree = BinaryDeviceTree::from_path("./test-files/bcm2711-raspberrypi-4b.dtb").unwrap();
    let properties = device_tree.find_node_by_alias("gpio").unwrap().properties().map(|(name, _)| name).collect::<Vec<_>>();
    assert!(!properties.contains(&"brcm,pins"), "Properties of children node should not be collected when enumerating properties");
}

#[test]
fn children_node_traversal() {
    let device_tree = BinaryDeviceTree::from_path("./test-files/bcm2711-raspberrypi-4b.dtb").unwrap();
    let children = device_tree.root_node().children().map(|node| node.name).collect::<Vec<_>>();
    assert!(!children.contains(&"gpio"), "Children of root's children nodes should not be collected when enumerating root node");
}

#[test]
fn illegal_header() {
    fn header_bytes<'a>(header: DtbHeader) -> &'a [u8] {
        unsafe {
            slice::from_raw_parts(&header as *const DtbHeader as *const u8, size_of::<DtbHeader>())
        }
    }

    assert_eq!(
        BinaryDeviceTree::from_slice(header_bytes(DtbHeader::default())),
        Err(Error::InvalidMagicBytes(0)),
        "Magic bytes should be invalid"
    );
    assert_eq!(
        BinaryDeviceTree::from_slice(header_bytes(DtbHeader { magic_bytes: 0xD00DFEEDu32.swap_bytes(), ..Default::default() })),
        Err(Error::InvalidTotalSize(0)),
        "Size should be invalid"
    );
    assert_eq!(
        BinaryDeviceTree::from_slice(header_bytes(DtbHeader {
            magic_bytes: 0xD00DFEEDu32.swap_bytes(),
            total_size: size_of::<DtbHeader>() as _,
            ..Default::default()
        })),
        Err(Error::IncompatibleVersion(17, 0)),
        "Version should be incompatible"
    );
}
