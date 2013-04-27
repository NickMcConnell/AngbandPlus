// sys-dos.h -- MJC

/*
 * This file contains some system-specific stuff for DOS systems.
 */

//#include <go32.h>
//#include <dpmi.h>
//#include <bios.h>
//#include <dos.h>
//#include <pc.h>
//#include <sys/farptr.h>

#define END_OF_FUNCTION(x)    void x##_end() { }

#define LOCK_VARIABLE(x)      _go32_dpmi_lock_data((void *)&x, sizeof(x))
#define LOCK_FUNCTION(x)      _go32_dpmi_lock_code(x, (long)x##_end - (long)x)

/* macros to enable and disable interrupts */
#define DISABLE()   asm volatile ("cli")
#define ENABLE()    asm volatile ("sti")

extern int install_timer(void);
extern void remove_timer(void);
extern int install_sound(int digi, int midi, char *cfg_path);
extern void remove_sound(void);

