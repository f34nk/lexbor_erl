cmake_minimum_required(VERSION 3.10)
project(lexbor_port C)
set(CMAKE_C_STANDARD 99)
set(CMAKE_BUILD_TYPE Release)

# Enable testing
enable_testing()

add_executable(lexbor_port port_main.c)
add_executable(port_main_test port_main_test.c)

# Lexbor discovery:
# 1) pkg-config (preferred when include/lib dirs are valid)
# 2) Homebrew prefixes (common paths, then `brew --prefix`)
# 3) System defaults
#
# Note: Homebrew's lexbor.pc for 3.x can ship with empty includedir/libdir.
# Treat that as "not found" and fall through to prefix-based discovery.

set(LEXBOR_INCLUDE_DIRS "")
set(LEXBOR_LIBRARY_DIRS "")
set(LEXBOR_LIBRARIES lexbor)

find_package(PkgConfig QUIET)
if(PkgConfig_FOUND)
  pkg_check_modules(LEXBOR_PC QUIET lexbor)
  if(LEXBOR_PC_FOUND AND LEXBOR_PC_INCLUDE_DIRS)
    set(LEXBOR_INCLUDE_DIRS ${LEXBOR_PC_INCLUDE_DIRS})
    set(LEXBOR_LIBRARY_DIRS ${LEXBOR_PC_LIBRARY_DIRS})
    if(LEXBOR_PC_LIBRARIES)
      set(LEXBOR_LIBRARIES ${LEXBOR_PC_LIBRARIES})
    endif()
    message(STATUS "Found lexbor via pkg-config (${LEXBOR_PC_VERSION})")
  endif()
endif()

if(NOT LEXBOR_INCLUDE_DIRS)
  set(_lexbor_prefixes "")
  if(DEFINED ENV{LEXBOR_PREFIX} AND NOT "$ENV{LEXBOR_PREFIX}" STREQUAL "")
    list(APPEND _lexbor_prefixes "$ENV{LEXBOR_PREFIX}")
  endif()
  list(APPEND _lexbor_prefixes
    /opt/homebrew/opt/lexbor
    /usr/local/opt/lexbor
    /opt/homebrew
    /usr/local
  )

  if(APPLE)
    execute_process(
      COMMAND brew --prefix lexbor
      OUTPUT_VARIABLE _brew_lexbor_prefix
      OUTPUT_STRIP_TRAILING_WHITESPACE
      ERROR_QUIET
    )
    if(_brew_lexbor_prefix)
      list(INSERT _lexbor_prefixes 0 "${_brew_lexbor_prefix}")
    endif()
  endif()

  foreach(_prefix IN LISTS _lexbor_prefixes)
    if(EXISTS "${_prefix}/include/lexbor/html/parser.h")
      set(LEXBOR_INCLUDE_DIRS "${_prefix}/include")
      set(LEXBOR_LIBRARY_DIRS "${_prefix}/lib")
      message(STATUS "Found lexbor headers at ${_prefix}")
      break()
    endif()
  endforeach()
endif()

if(LEXBOR_INCLUDE_DIRS)
  target_include_directories(lexbor_port PRIVATE ${LEXBOR_INCLUDE_DIRS})
  target_include_directories(port_main_test PRIVATE ${LEXBOR_INCLUDE_DIRS})
endif()

if(LEXBOR_LIBRARY_DIRS)
  target_link_directories(lexbor_port PRIVATE ${LEXBOR_LIBRARY_DIRS})
  target_link_directories(port_main_test PRIVATE ${LEXBOR_LIBRARY_DIRS})
endif()

target_link_libraries(lexbor_port ${LEXBOR_LIBRARIES})
target_link_libraries(port_main_test ${LEXBOR_LIBRARIES})

# Add test
add_test(NAME port_main_tests COMMAND port_main_test)
