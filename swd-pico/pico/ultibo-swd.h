#include <stdbool.h>
#include <stddef.h>

extern bool Halt (void);

extern bool LoadChunk (const void* *pChunk, size_t nChunkSize, unsigned int nAddress);

bool Start (unsigned int nAddress);

bool ultibo_Halt(void);

bool ultibo_LoadChunk (const void*  *pChunk, size_t nChunkSize, unsigned int nAddress);

bool Ulitbo_Start (unsigned int nAddress);

bool Initialize (void);

bool ultibo_Initialize(void);