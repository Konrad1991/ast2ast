#ifndef HEAP_SIZE_ETR_H
#define HEAP_SIZE_ETR_H

namespace etr {

#ifdef _WIN32

#include <windows.h>
unsigned long long getTotalSystemMemory() {
  MEMORYSTATUSEX status;
  status.dwLength = sizeof(status);
  GlobalMemoryStatusEx(&status);
  return status.ullTotalPhys;
}

#elif __linux__
// https://stackoverflow.com/questions/2513505/how-to-get-available-memory-c-g
#include <unistd.h>

unsigned long long getTotalSystemMemory() {
  long pages = sysconf(_SC_PHYS_PAGES);
  long page_size = sysconf(_SC_PAGE_SIZE);
  return pages * page_size;
}

#elif __APPLE__

##include<unistd.h>

    unsigned long long getTotalSystemMemory() {
  long pages = sysconf(_SC_PHYS_PAGES);
  long page_size = sysconf(_SC_PAGE_SIZE);
  return pages * page_size;
}

#endif

} // namespace etr

#endif