use crate::{DtbHeader, StructureBlockProperty, StructureBlockPropertyIterator, StructureBlockToken, StructureBlockTokenIterator};
use crate::stack::Stack;

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub struct StructureBlockNode<'a> {
    pub(crate) data: &'a [u8],
    pub(crate) strings_data: &'a [u8],
    pub(crate) header: &'a DtbHeader,
    pub name: &'a str
}

impl<'a> StructureBlockNode<'a> {
    /// This function enumerates all children of this structure block node and tries to find a children. When the name of the node contains
    /// the address (the node name has a '@' separator), we only look for the first part before the separator. When the expected name
    /// contains a '@', we check for the full name without separation.
    ///
    /// ## Note
    /// - When calling this function, everytime we create a new iterator and enumerate through the memory section
    #[inline(always)]
    pub fn find_child(&self, name: &str) -> Option<StructureBlockNode<'a>> {
        if name.contains("@") {
            self.children().find(|node| node.name == name)
        } else {
            self.children().find(|node| node.name.split("@").next().unwrap_or(node.name) == name)
        }
    }

    #[inline(always)]
    pub fn find_property(&self, requested_name: &str) -> Option<StructureBlockProperty<'a>> {
        self.properties().find(|(name, _)| *name == requested_name).map(|(_, property)| property)
    }

    /// This function returns an iterator over all children nodes for this node in the range of the data of this node.
    #[inline(always)]
    pub fn children(&self) -> StructureBlockNodeIterator<'a> {
        self.node_iterator(true)
    }

    pub fn properties(&self) -> StructureBlockPropertyIterator<'a> {
        StructureBlockPropertyIterator {
            inner: self.token_iterator(),
            depth: 0
        }
    }

    #[inline(always)]
    fn token_iterator(&self) -> StructureBlockTokenIterator<'a> {
        StructureBlockTokenIterator {
            remaining_data: self.data,
            strings_data: self.strings_data,
            header: self.header,
            node_stack: Stack::default(),
        }
    }

    #[inline(always)]
    pub(crate) fn node_iterator(&self, only_node_children: bool) -> StructureBlockNodeIterator<'a> {
        StructureBlockNodeIterator {
            inner: self.token_iterator(),
            node_stack: Stack::default(),
            only_node_children
        }
    }
}

/// This struct is implementing an iterator for structure block node children of another structure block node. This is used for the
/// iteration of node children and filtering them.
#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub struct StructureBlockNodeIterator<'a> {
    inner: StructureBlockTokenIterator<'a>,
    node_stack: Stack<(&'a str, &'a [u8]), 10>, // TODO: We want to support stacks (allocated on the heap) when alloc crate is present
    only_node_children: bool
}

impl<'a> Iterator for StructureBlockNodeIterator<'a> {
    type Item = StructureBlockNode<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut remaining_data = self.inner.remaining_data;
        while let Some(token) = self.inner.next() {
            match token {
                StructureBlockToken::BeginNode(name) => self.node_stack.push((name, remaining_data)).ok()?,
                StructureBlockToken::EndNode => {
                    let top_index = self.node_stack.top_index();
                    if top_index == 1 {
                        return None;
                    }

                    let (name, node_start) = self.node_stack.pop()?;
                    let length = node_start.len() - self.inner.remaining_data.len();
                    if !self.only_node_children {
                        return Some(StructureBlockNode {
                            data: &node_start[0..length],
                            strings_data: self.inner.strings_data,
                            header: self.inner.header,
                            name
                        });
                    }


                    if top_index == 2 {
                        let length = node_start.len() - self.inner.remaining_data.len();
                        return Some(StructureBlockNode {
                            data: &node_start[0..length],
                            strings_data: self.inner.strings_data,
                            header: self.inner.header,
                            name
                        });
                    }
                }
                _ => {}
            }
            remaining_data = self.inner.remaining_data;
        }

        None
    }
}
