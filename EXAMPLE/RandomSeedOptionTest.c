#include <limits.h>
#include <stdio.h>
#include "dBPACK_wrapper.h"
#include "zBPACK_wrapper.h"
#include "sBPACK_wrapper.h"
#include "cBPACK_wrapper.h"

typedef void (*CreateOption)(F2Cptr*);
typedef void (*SetOption)(F2Cptr*, const char*, C2Fptr);
typedef void (*GetOption)(F2Cptr*, const char*, double*);
typedef void (*CopyOption)(F2Cptr*, F2Cptr*);
typedef void (*DeleteOption)(F2Cptr*);

static int check_options(CreateOption create, SetOption set, GetOption get,
                         CopyOption copy, DeleteOption destroy) {
    F2Cptr option = NULL;
    double value;
    int failures = 0;
    int seeds[] = {0, 12345, INT_MAX, -1};
    create(&option);
    get(&option, "random_seed", &value);
    if (value != -1) ++failures;
    for (unsigned i = 0; i < sizeof(seeds) / sizeof(seeds[0]); ++i) {
        F2Cptr copied = NULL;
        set(&option, "random_seed", &seeds[i]);
        get(&option, "random_seed", &value);
        if (value != seeds[i]) ++failures;
        copy(&option, &copied);
        get(&copied, "random_seed", &value);
        if (value != seeds[i]) ++failures;
        destroy(&copied);
    }
    destroy(&option);
    return failures;
}

#define CHECK(prefix) check_options(prefix##_c_bpack_createoption, \
    prefix##_c_bpack_setoption, prefix##_c_bpack_getoption, \
    prefix##_c_bpack_copyoption, prefix##_c_bpack_deleteoption)

int main(void) {
    int failures = CHECK(d) + CHECK(z) + CHECK(s) + CHECK(c);
    printf("C random_seed option regression: %s\n", failures ? "FAIL" : "PASS");
    return failures ? 1 : 0;
}
