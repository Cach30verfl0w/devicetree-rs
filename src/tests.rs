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

#[test]
fn memory_mapping() {
    let device_tree = BinaryDeviceTree::from_path("./test-files/bcm2711-raspberrypi-4b.dtb").unwrap();
    let actual_memory_mapping = device_tree.bus_address_spaces_map().unwrap().collect::<Vec<_>>();
    let expected_memory_mapping: [(u64, u64, u64); 3] = [
        (0x7E000000, 0xFE000000, 0x01800000),
        (0x7C000000, 0xFC000000, 0x02000000),
        (0x40000000, 0xFF800000, 0x00800000)
    ];

    assert_eq!(expected_memory_mapping, actual_memory_mapping.as_ref());
}
