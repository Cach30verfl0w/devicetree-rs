use crate::{StructureBlockToken, StructureBlockTokenIterator};

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub enum StructureBlockProperty<'a> {
    String(&'a str),
    UnsignedInt32(u32),
    RawData(&'a [u8])
}

impl<'a> StructureBlockProperty<'a> {
    #[inline(always)]
    pub fn as_u32(&self) -> Option<u32> { // TODO If RawData, try to create u32 from it
        Some(match self {
            Self::UnsignedInt32(value) => *value,
            _ => return None
        })
    }

    #[inline(always)]
    pub fn as_bytes(&self) -> Option<&'a [u8]> {
        Some(match self {
            Self::RawData(data) => data,
            Self::String(data) => data.as_bytes(),
            _ => return None
        })
    }

    #[inline(always)]
    pub fn as_str(&self) -> Option<&'a str> { // TODO: If RawData, try to format
        Some(match self {
            Self::String(data) => data,
            _ => return None
        })
    }

    #[inline(always)]
    pub fn as_ptr(&self) -> Option<*const u8> {
        Some(match self {
            Self::String(value) => value.as_ptr(),
            Self::RawData(value) => value.as_ptr(),
            _ => return None
        })
    }
}

/// This struct is implementing an iterator for structure block node properties of a structure block node. This is used for the iteration of
/// properties and filtering them.
#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub struct StructureBlockPropertyIterator<'a> {
    pub(crate) inner: StructureBlockTokenIterator<'a>,
    pub(crate) depth: u32
}

impl<'a> Iterator for StructureBlockPropertyIterator<'a> {
    type Item = (&'a str, StructureBlockProperty<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(token) = self.inner.next() {
            match token {
                StructureBlockToken::BeginNode(_) => {
                    self.depth += 1;
                },
                StructureBlockToken::Property { name, data } => {
                    if self.depth == 1 {
                        return Some((name, data));
                    }
                }
                StructureBlockToken::EndNode => {
                    self.depth -= 1;
                    if self.depth == 0 {
                        return None;
                    }
                }
                _ => {}
            }
        }

        None
    }
}

