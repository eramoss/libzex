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

typedef enum {
  REG_OK = 0,		/* No error. */
  /* POSIX tre_regcomp() return error codes.  (In the order listed in the
     standard.)	 */
  REG_NOMATCH,		/* No match. */
  REG_BADPAT,		/* Invalid regexp. */
  REG_ECOLLATE,		/* Unknown collating element. */
  REG_ECTYPE,		/* Unknown character class name. */
  REG_EESCAPE,		/* Trailing backslash. */
  REG_ESUBREG,		/* Invalid back reference. */
  REG_EBRACK,		/* "[]" imbalance */
  REG_EPAREN,		/* "\(\)" or "()" imbalance */
  REG_EBRACE,		/* "\{\}" or "{}" imbalance */
  REG_BADBR,		/* Invalid content of {} */
  REG_ERANGE,		/* Invalid use of range operator */
  REG_ESPACE,		/* Out of memory.  */
  REG_BADRPT,		/* Invalid use of repetition operators. */
  REG_BADMAX,		/* Maximum repetition in {} too large */
} reg_errcode_t;

// Function prototypes
int32_t regcomp(regex_t *preg, const char *regex, int32_t cflags);
int32_t regexec(const regex_t *preg, const char *string, uint32_t nmatch, regmatch_t *pmatch, int32_t eflags);
uint32_t regerror(int32_t errcode, const regex_t *preg, char *effbuf, uint32_t errbuf_size);
void regfree(regex_t *preg);

