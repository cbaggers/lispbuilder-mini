/* Copyright (c) 2009, Luke J Crook.  All rights reserved. 

   Redistribution and use in source and binary forms, with or without 
   modification, are permitted provided that the following conditions 
   are met: 

     * Redistributions of source code must retain the above copyright 
       notice, this list of conditions and the following disclaimer. 

     * Redistributions in binary form must reproduce the above 
       copyright notice, this list of conditions and the following 
       disclaimer in the documentation and/or other materials 
       provided with the distribution. 

   THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED 
   OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
   ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY 
   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */

#include <SDL.h>

// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the lbmSDLGLUE_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// lbmSDLGLUE_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifdef WIN32
#define lbmSDLGLUE_API extern __declspec(dllexport)
#else
#define lbmSDLGLUE_API extern DECLSPEC
#endif 

/* Set up for C function definitions, even when using C++ */
#ifdef __cplusplus
extern "C" {
#endif

/*	lbm-sdl checks this variable to determine if the audio buffer is empty */
SDL_mutex* buffer_fill_lock = NULL; //SDL_CreateMutex();
int buffer_fill = -1;

/*	The glue wrapper sleeps on this semaphore until lbm-sdl has filled the audio buffer. */
SDL_sem* audio_buffer_lock = NULL; //SDL_CreateSemaphore(1);

/*	This global variable holds an audio buffer that is used to transfer audio data between
	lbm-sdl and the callback */
Uint8* audio_buffer = NULL;
int audio_buffer_length = 0;

lbmSDLGLUE_API int SDLCALL SDL_glue_SDL_WaitUntilBufferFull(void);
lbmSDLGLUE_API int SDLCALL SDL_glue_SDL_BufferFilled(void);
lbmSDLGLUE_API int SDLCALL SDL_glue_SDL_RequireBufferFill(void);

lbmSDLGLUE_API Uint8* SDLCALL SDL_glue_SDL_GetAudioBuffer(void);
lbmSDLGLUE_API int SDLCALL SDL_glue_SDL_GetAudioBufferLength(void);

lbmSDLGLUE_API void SDLCALL SDL_glue_mixaudio(void *udata, Uint8 *stream, int len);
lbmSDLGLUE_API int SDLCALL SDL_glue_SDL_OpenAudio(SDL_AudioSpec *desired, SDL_AudioSpec *obtained);
lbmSDLGLUE_API void SDLCALL SDL_glue_SDL_CloseAudio(void);

lbmSDLGLUE_API void (SDLCALL *callback)(void *userdata, Uint8 *stream, int len);

/* Ends C function definitions when using C++ */
#ifdef __cplusplus
}
#endif

