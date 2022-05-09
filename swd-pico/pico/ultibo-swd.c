#include "ultibo-swd.h"

bool ultibo_Halt(void) {
bool resultof;
	resultof = Halt();
  return(resultof);
}

bool ultibo_Initialize(void) {
bool resultof;
	resultof = Initialize();
  return(resultof);
}

bool ultibo_LoadChunk (const void* *pChunk, size_t nChunkSize, unsigned int nAddress) {
bool resultof;
	resultof = LoadChunk(*pChunk,nChunkSize,nAddress);
  return(resultof);
}

bool Ulitbo_Start (unsigned int nAddress) {
bool resultof;
	resultof = Start(nAddress);
  return(resultof);
}
