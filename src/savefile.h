#ifndef SAVEFILE_H
#define SAVEFILE_H

typedef struct version_s {
    byte major;
    byte minor;
    byte patch;
    byte extra;
} version_t, *version_ptr;

extern int version_compare(version_ptr left, version_ptr right);

enum {
    SAVEFILE_READ,
    SAVEFILE_WRITE
};

typedef struct savefile_s {
    FILE     *file;
    int       type;     /* READ or WRITE */
    byte      xor_byte;
    u32b      v_check;
    u32b      x_check;
    version_t version;
    int       pos;
} savefile_t, *savefile_ptr;

extern savefile_ptr savefile_open_read(const char *name);
extern savefile_ptr savefile_open_write(const char *name);
extern bool         savefile_close(savefile_ptr file);

extern bool         savefile_is_older_than(savefile_ptr file, byte major, byte minor, byte patch, byte extra);

extern byte         savefile_read_byte(savefile_ptr file);
extern bool         savefile_read_bool(savefile_ptr file);
extern u16b         savefile_read_u16b(savefile_ptr file);
extern s16b         savefile_read_s16b(savefile_ptr file);
extern u32b         savefile_read_u32b(savefile_ptr file);
extern s32b         savefile_read_s32b(savefile_ptr file);
extern void         savefile_read_cptr(savefile_ptr file, char *buf, int max);
extern string_ptr   savefile_read_string(savefile_ptr file);
extern void         savefile_read_skip(savefile_ptr file, int cb);

extern void         savefile_write_byte(savefile_ptr file, byte v);
extern void         savefile_write_bool(savefile_ptr file, bool v);
extern void         savefile_write_u16b(savefile_ptr file, u16b v);
extern void         savefile_write_s16b(savefile_ptr file, s16b v);
extern void         savefile_write_u32b(savefile_ptr file, u32b v);
extern void         savefile_write_s32b(savefile_ptr file, s32b v);
extern void         savefile_write_cptr(savefile_ptr file, const char *buf);

#endif
