/*
 * Voodoo board information program.
 * Copyright (c) 2020 Khralkatorrix
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>

#include <glide.h>

HMODULE hGlide3;

typedef void (FX_CALL *grGlideInitProc)(void);
typedef void (FX_CALL *grGlideShutdownProc)(void);
typedef FxU32 (FX_CALL *grGetProc)(FxU32 pname, FxU32 plength, FxI32 *params);
typedef const char* (FX_CALL *grGetStringProc)(FxU32 pname);

grGlideInitProc         pgrGlideInit;
grGlideShutdownProc     pgrGlideShutdown;
grGetProc               pgrGet;
grGetStringProc         pgrGetString;

int main(int argc, char **argv)
{
    char *hardware;
    char *renderer;
    char *vendor;
    char *version;
    char *extensions;

    FxI32 num_board = 0;
    FxI32 num_fbi = 0;
    FxI32 fb_rev = 0;
    FxI32 fb_mem = 0;
    FxI32 num_tmu = 0;
    FxI32 tmu_rev = 0;
    FxI32 tmu_mem = 0;
    FxI32 max_tex_size = 0;

    hGlide3 = LoadLibraryA("glide3x.dll");
    if (!hGlide3)
    {
        fprintf(stderr, "No Glide installed or missing glide3x.dll.\n");
        return 1;
    }

    pgrGlideInit = (grGlideInitProc)GetProcAddress(hGlide3, "_grGlideInit@0");
    if (!pgrGlideInit)
    {
        fprintf(stderr, "glide3x.dll is missing grGlideInit function!\n");
        FreeLibrary(hGlide3);
        return 1;
    }

    pgrGlideShutdown = (grGlideShutdownProc)GetProcAddress(hGlide3, "_grGlideShutdown@0");
    if (!pgrGlideShutdown)
    {
        fprintf(stderr, "glide3x.dll is missing grGlideShutdown function!\n");
        FreeLibrary(hGlide3);
        return 1;
    }

    pgrGet = (grGetProc)GetProcAddress(hGlide3, "_grGet@12");
    if (!pgrGet)
    {
        fprintf(stderr, "glide3x.dll is missing grGet function!\n");
        FreeLibrary(hGlide3);
        return 1;
    }

    pgrGetString = (grGetStringProc)GetProcAddress(hGlide3, "_grGetString@4");
    if (!pgrGetString)
    {
        fprintf(stderr, "glide3x.dll is missing grGetString function!\n");
        FreeLibrary(hGlide3);
        return 1;
    }

    /* Initialize Glide */
    pgrGlideInit();

    /* Get Voodoo board type */
    hardware = pgrGetString(GR_HARDWARE);
    /* Get glide driver vender */
    vendor = pgrGetString(GR_VENDOR);
    /* Get renderer */
    renderer = pgrGetString(GR_RENDERER);
    /* Get glide driver version */
    version = pgrGetString(GR_VERSION);
    /* Get glide extensions */
    extensions = pgrGetString(GR_EXTENSION);

    printf("Glide hardware string: %s\n", hardware);
    printf("Glide vendor string: %s\n", vendor);
    printf("Glide renderer string: %s\n", renderer);
    printf("Glide version string: %s\n", version);
    printf("Glide extensions: \n%s\n", extensions);

    /* Number of board(s) */
    pgrGet(GR_NUM_BOARDS, sizeof(num_board), &num_board);
    /* Frame Buffer Interface(s) */
    pgrGet(GR_NUM_FB, sizeof(num_fbi), &num_fbi);
    /* FBI Revision */
    pgrGet(GR_REVISION_FB, sizeof(fb_rev), &fb_rev);
    /* Frame Buffer Memory */
    pgrGet(GR_MEMORY_FB, sizeof(fb_mem), &fb_mem);
    /* Texture Mapping Unit(s) */
    pgrGet(GR_NUM_TMU, sizeof(num_tmu), &num_tmu);
    /* TMU Revision */
    pgrGet(GR_REVISION_TMU, sizeof(tmu_rev), &tmu_rev);
    /* Total Texture Memory */
    pgrGet(GR_MEMORY_TMU, sizeof(tmu_mem), &tmu_mem);
    /* Total Texture Memory */
    pgrGet(GR_MAX_TEXTURE_SIZE, sizeof(max_tex_size), &max_tex_size);

    printf("Scanline Interleave (SLI): %s\n", num_fbi - 1 ? "Yes" : "No");
    printf("Number of board(s): %d\n", num_board);
    printf("Frame Buffer Interface(s): %d\n", num_fbi);
    printf("FBI revision: %d\n", fb_rev);
    printf("Frame Buffer memory: %d MB\n", fb_mem / 1048576);
    printf("Texture Mapping Unit(s): %d\n", num_tmu);
    printf("TMU revision: %d\n", tmu_rev);
    printf("Texture memory per TMU: %d MB\n", tmu_mem / 1048576);
    printf("Max texture size: %dx%d pixels\n", max_tex_size, max_tex_size);

    pgrGlideShutdown();
    FreeLibrary(hGlide3);
    return 0;
}
