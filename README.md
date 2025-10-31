# devicetree-rs
This crate implements a no-std compatible, pure-Rust library for parsing the Binary Devicetree (DTB) format. It provides the following features:
- **no_std Support:** Support for running on no-std environments for usage on embedded systems
- **Zero Copy:** This library doesn't copy the tree or creates allocated structures based on it. The parser works with views on the binary data
- **Compliance:** Compliance with the Devicetree Specification v0.4

## ToDo
- [ ] Testing against malformed formats (fuzzing etc.)

## License
This project is licensed under the **Apache-2.0** license.
```
Copyright 2025 Cedric Hammes

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	https://www.apache.org/licenses/LICENSE-2.0
	
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

The tests (`src/tests.rs`) are licensed under the `GNU General Public License 2.0`. The device tree files used in the 
tests originate from the Linux kernel and are also licensed under GPL-2.0. Please ensure that you comply with the terms 
of the GPL-2.0 license if you use or distribute these test files.
