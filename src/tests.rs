// SPDX-License-Identifier: GPL-2.0
//
// SPDX-FileCopyrightText: 2025 Cedric Hammes <contact@cach30verfl0w.net>

use crate::BinaryDeviceTree;

#[test]
fn node_enumeration() {
    let device_tree = BinaryDeviceTree::from_path("./test-files/bcm2711-raspberrypi-4b.dtb").unwrap();
    let Some(soc_node) = device_tree.root_node().find_child("soc") else {
        panic!("Unable to find SoC node");
    };

    assert_eq!(soc_node.children().collect::<Vec<_>>().len(), 67);
}
