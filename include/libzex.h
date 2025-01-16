#pragma once
#include <stdint.h>

typedef struct {
    uint32_t re_nsub;      
    void *value;           // Internal use only (pointer to Regex structure)
} regex_t;

typedef struct {
    int32_t rm_so;        
    int32_t rm_eo;       
} regmatch_t;

typedef int32_t regoff_t;

// Function prototypes
int32_t regcomp(regex_t *preg, const char *regex, int32_t cflags);
int32_t regexec(const regex_t *preg, const char *string, uint32_t nmatch, regmatch_t *pmatch, int32_t eflags);
uint32_t regerror(int32_t errcode, const regex_t *preg, char *effbuf, uint32_t errbuf_size);
void regfree(regex_t *preg);

