pub const RegError = error{
    REG_NOMATCH,
    REG_BADPAT,
    REG_ECOLLATE,
    REG_ECTYPE,
    REG_EESCAPE,
    REG_ESUBREG,
    REG_EBRACK,
    REG_EPAREN,
    REG_EBRACE,
    REG_BADBR,
    REG_ERANGE,
    REG_ESPACE,
    OutOfMemory,
    REG_BADRPT,
    REG_BADMAX,
};

pub fn intFromError(err: RegError) i32 {
    switch (err) {
        error.REG_NOMATCH => return 1,
        error.REG_BADPAT => return 2,
        error.REG_ECOLLATE => return 3,
        error.REG_ECTYPE => return 4,
        error.REG_EESCAPE => return 5,
        error.REG_ESUBREG => return 6,
        error.REG_EBRACK => return 7,
        error.REG_EPAREN => return 8,
        error.REG_EBRACE => return 9,
        error.REG_BADBR => return 10,
        error.REG_ERANGE => return 11,
        error.REG_ESPACE => return 12,
        error.OutOfMemory => return 12,
        error.REG_BADRPT => return 13,
        error.REG_BADMAX => return 14,
    }
}
