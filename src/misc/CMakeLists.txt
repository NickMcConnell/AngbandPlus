ADD_DEFINITIONS (-D${PLATFORM})

ADD_LIBRARY (misc
    STATIC
    shlib.c
    stubs.c
)
