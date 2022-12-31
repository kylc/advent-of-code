project(day3)
cmake_minimum_required(VERSION 3.14.159265358979323846)

function(intersection a b out)
  # A ∪ B
  list(APPEND a_union_b ${a} ${b})
  list(REMOVE_DUPLICATES a_union_b)

  # A ∖ B
  list(APPEND a_minus_b ${a})
  list(REMOVE_ITEM a_minus_b ${b})

  # B ∖ A
  list(APPEND b_minus_a ${b})
  list(REMOVE_ITEM b_minus_a ${a})

  # A ⊖ B = (A - B) ∪ (B ∖ A)
  list(APPEND a_sym_diff_b ${a_minus_b} ${b_minus_a})

  # A ∩ B = (A ∪ B) ∖ (A ⊖ B)
  list(APPEND a_int_b ${a_union_b})
  list(REMOVE_ITEM a_int_b ${a_sym_diff_b})

  set(${out} ${a_int_b} PARENT_SCOPE)
endfunction()

# CMake has no function for char -> int?
function(priority x out)
  # [a-z] -> 1..=26
  foreach(i RANGE 97 122)
    string(ASCII ${i} char)
    if(char STREQUAL x)
      math(EXPR value "${i} - 97 + 1")
      set(${out} ${value} PARENT_SCOPE)
    endif()
  endforeach()

  # [A-Z] -> 27..=52
  foreach(i RANGE 65 90)
    string(ASCII ${i} char)
    if(char STREQUAL x)
      math(EXPR value "${i} - 65 + 27")
      set(${out} ${value} PARENT_SCOPE)
    endif()
  endforeach()
endfunction()

file(STRINGS "data/input-3.txt" lines)

# p1
foreach(line ${lines})
  # Split rucksack into two compartments
  string(LENGTH ${line} line_len)
  math(EXPR line_midpoint "${line_len} / 2")
  string(SUBSTRING ${line} 0 ${line_midpoint} comp_a)
  string(SUBSTRING ${line} ${line_midpoint} -1 comp_b)

  # Extract characters as list
  string(REGEX MATCHALL "." a_chars ${comp_a})
  string(REGEX MATCHALL "." b_chars ${comp_b})

  # Compute set intersection
  intersection("${a_chars}" "${b_chars}" shared_item)

  # Compute and sum priority
  priority(${shared_item} prio)
  math(EXPR p1 "${p1} + ${prio}")
endforeach()

message(${p1})

# p2
list(LENGTH lines num_lines)
math(EXPR n "${num_lines} / 3 - 1")
foreach(i RANGE 0 ${n})
  # Index in groups of three
  math(EXPR k0 "${i} * 3 + 0")
  math(EXPR k1 "${i} * 3 + 1")
  math(EXPR k2 "${i} * 3 + 2")

  list(GET lines ${k0} s0)
  list(GET lines ${k1} s1)
  list(GET lines ${k2} s2)

  # Convert strings to lists
  string(REGEX MATCHALL "." c0 ${s0})
  string(REGEX MATCHALL "." c1 ${s1})
  string(REGEX MATCHALL "." c2 ${s2})

  # Compute (c0 ∩ c1) ∩ c2
  intersection("${c0}" "${c1}" c0_int_c1)
  intersection("${c0_int_c1}" "${c2}" badge)

  # Sum total priority
  priority(${badge} prio)
  math(EXPR p2 "${p2} + ${prio}")
endforeach()

message(${p2})
