cmake_minimum_required(VERSION 3.10)
project(lexbor_port C)
set(CMAKE_C_STANDARD 99)
set(CMAKE_BUILD_TYPE Release)

# Enable testing
enable_testing()

add_executable(lexbor_port port_main.c)
add_executable(port_main_test port_main_test.c)

# Try pkg-config first
find_package(PkgConfig QUIET)
if(PkgConfig_FOUND)
  pkg_check_modules(LEXBOR QUIET lexbor)
endif()

if(LEXBOR_FOUND)
  target_include_directories(lexbor_port PRIVATE ${LEXBOR_INCLUDE_DIRS})
  target_link_libraries(lexbor_port ${LEXBOR_LIBRARIES})
  target_include_directories(port_main_test PRIVATE ${LEXBOR_INCLUDE_DIRS})
  target_link_libraries(port_main_test ${LEXBOR_LIBRARIES})
else()
  # Try Homebrew on macOS
  if(APPLE)
    execute_process(
      COMMAND brew --prefix lexbor
      OUTPUT_VARIABLE LEXBOR_PREFIX
      OUTPUT_STRIP_TRAILING_WHITESPACE
      ERROR_QUIET
    )
    if(LEXBOR_PREFIX)
      target_include_directories(lexbor_port PRIVATE ${LEXBOR_PREFIX}/include)
      target_link_directories(lexbor_port PRIVATE ${LEXBOR_PREFIX}/lib)
      target_link_libraries(lexbor_port lexbor)
      target_include_directories(port_main_test PRIVATE ${LEXBOR_PREFIX}/include)
      target_link_directories(port_main_test PRIVATE ${LEXBOR_PREFIX}/lib)
      target_link_libraries(port_main_test lexbor)
    else()
      # Fallback: assume system-wide installation
      target_link_libraries(lexbor_port lexbor)
      target_link_libraries(port_main_test lexbor)
    endif()
  else()
    # Linux/Unix: assume system-wide installation
    target_link_libraries(lexbor_port lexbor)
    target_link_libraries(port_main_test lexbor)
  endif()
endif()

# Add test
add_test(NAME port_main_tests COMMAND port_main_test)
