use crate::{
    BinaryDeviceTree,
    DtbHeader,
    StructureBlockProperty,
    structures::property::StructureBlockPropertyIterator,
    StructureBlockToken,
    StructureBlockTokenIterator
};
use crate::stack::Stack;

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub struct StructureBlockNode<'a> {
    pub(crate) data: &'a [u8],
    pub(crate) strings_data: &'a [u8],
    pub(crate) header: &'a DtbHeader,
    pub name: &'a str
}

impl<'a> StructureBlockNode<'a> {
    #[inline(always)]
    pub fn find_child(&self, name: &str) -> Option<StructureBlockNode<'a>> {
        self.children().find(|node| node.name == name)
    }

    #[inline(always)]
    pub fn find_children(&self, expected_name: &str) -> impl Iterator<Item = StructureBlockNode<'a>> {
        self.children().filter(move |node| node.name.split("@").next().unwrap_or(node.name) == expected_name)
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

impl<'a> BinaryDeviceTree<'a> {
    pub fn find_node_by_alias(&'a self, alias: &str) -> Option<StructureBlockNode<'a>> {
        let mut current_node = self.root_node();
        let alias_property = current_node.find_child("aliases")?.find_property(alias)?.as_str()?;
        for path_element in alias_property[1..alias_property.len()].split("/") {
            current_node = current_node.find_child(path_element)?;
        }

        Some(current_node)
    }

    /// This function performs a reverse lookup over the aliases table to find the alias for the specified value pointed at. As example, if
    /// you enter '/soc/serial@00000000' and the alias is 'serial0', this function returns 'serial0'.
    pub fn find_alias_by_path(&'a self, path: &str) -> Option<&'a str> {
        self.root_node().find_child("aliases")?.properties()
            .filter(|(_, value)| value.as_str().map(|x| x == path).unwrap_or(false))
            .map(|(name, _)| name)
            .next()
    }

    /// This function returns the root node in the structure block section. This can be used to iterate over all nodes present in the
    /// structure block.
    pub fn root_node(&'a self) -> StructureBlockNode<'a> {
        let data = self.data.as_ref();
        StructureBlockNode {
            data: &data[self.header.struct_block_offset..(self.header.struct_block_offset + self.header.struct_block_size)],
            strings_data: &data[self.header.strings_block_offset..(self.header.strings_block_offset + self.header.strings_block_size)],
            header: &self.header,
            name: ""
        }
    }

    pub fn find_node_by_phandle(&'a self, phandle: u32) -> Option<StructureBlockNode<'a>> {
        self.root_node().node_iterator(false).find(|node| {
            if let Some(node_phandle) = node.find_property("phandle").and_then(|x| x.as_u32()) && node_phandle == phandle {
                return true;
            };
            return false;
        })
    }
}
