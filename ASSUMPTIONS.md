## Assumptions

* Node values can be represented by integers
* Input will fit into memory
  * Provided sample file contains 2000 lines, which leaves us with 4bytes * 2000 * 2000 / 2 = 8000000 bytes of actual working data. Increasing the file to 20'000 entries would produce 1.6 GB of working data which is not too extreme.
  * Task requirements specify 500 lines
