/*
 * 3Dfx Voodoo/Voodoo2 3D Accelerator Board Driver
 *
 * Debug macro
 */

#if !defined(__DEBUG_H__)
#define __DEBUG_H__

#if DBG
#define DebugPrint(_x_) \
            DbgPrint ("FXPTL:"); \
            DbgPrint _x_;
#else
#define DebugPrint(_x_)
#endif

#endif /* __FXPTL_H__ */
