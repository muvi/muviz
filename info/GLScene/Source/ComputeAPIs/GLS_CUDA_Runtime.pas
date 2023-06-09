//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLS_CUDA_Runtime<p>

   <b>History : </b><font size=-1><ul>
      <li>28/01/10 - Yar - Creation
   </ul></font>
}

///*
// * Copyright 1993-2009 NVIDIA Corporation.  All rights reserved.
// *
// * NOTICE TO USER:
// *
// * This source code is subject to NVIDIA ownership rights under U.S. and
// * international Copyright laws.  Users and possessors of this source code
// * are hereby granted a nonexclusive, royalty-free license to use this code
// * in individual and commercial software.
// *
// * NVIDIA MAKES NO REPRESENTATION ABOUT THE SUITABILITY OF THIS SOURCE
// * CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR
// * IMPLIED WARRANTY OF ANY KIND.  NVIDIA DISCLAIMS ALL WARRANTIES WITH
// * REGARD TO THIS SOURCE CODE, INCLUDING ALL IMPLIED WARRANTIES OF
// * MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR PURPOSE.
// * IN NO EVENT SHALL NVIDIA BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL,
// * OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
// * OF USE, DATA OR PROFITS,  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
// * OR OTHER TORTIOUS ACTION,  ARISING OUT OF OR IN CONNECTION WITH THE USE
// * OR PERFORMANCE OF THIS SOURCE CODE.
// *
// * U.S. Government End Users.   This source code is a "commercial item" as
// * that term is defined at  48 C.F.R. 2.101 (OCT 1995), consisting  of
// * "commercial computer  software"  and "commercial computer software
// * documentation" as such terms are  used in 48 C.F.R. 12.212 (SEPT 1995)
// * and is provided to the U.S. Government only as a commercial end item.
// * Consistent with 48 C.F.R.12.212 and 48 C.F.R. 227.7202-1 through
// * 227.7202-4 (JUNE 1995), all U.S. Government End Users acquire the
// * source code with only those rights set forth herein.
// *
// * Any use of this source code in individual and commercial software must
// * include, in the user documentation and internal comments to the code,
// * the above Disclaimer and U.S. Government End Users Notice.
// */

unit GLS_CUDA_Runtime;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  GLS_CL_Platform,
  GLS_CUDA_API,
  OpenGL1x;

{$I cuda.inc}

const
  CUDARTDLLNAMES: array[0..5] of string = (
    'cudart32_31_4',
    'cudart32_30_14',
    'cudart32_30_9',
    'cudart32_30_8',
    'cudart32',
    'cudart'
    );

const
  // single precision constants
  CUDART_INF_F: Single = $7F800000;
  CUDART_NAN_F: Single = $7FFFFFFF;
  CUDART_MIN_DENORM_F: Single = $00000001;
  CUDART_MAX_NORMAL_F: Single = $7F7FFFFF;
  CUDART_NEG_ZERO_F: Single = $80000000;
  CUDART_ZERO_F = 0.0;
  CUDART_ONE_F = 1.0;
  CUDART_SQRT_HALF_F = 0.707106781;
  CUDART_SQRT_TWO_F = 1.414213562;
  CUDART_THIRD_F = 0.333333333;
  CUDART_PIO4_F = 0.785398163;
  CUDART_PIO2_F = 1.570796327;
  CUDART_3PIO4_F = 2.356194490;
  CUDART_2_OVER_PI_F = 0.636619772;
  CUDART_PI_F = 3.141592654;
  CUDART_L2E_F = 1.442695041;
  CUDART_L2T_F = 3.321928094;
  CUDART_LG2_F = 0.301029996;
  CUDART_LGE_F = 0.434294482;
  CUDART_LN2_F = 0.693147181;
  CUDART_LNT_F = 2.302585093;
  CUDART_LNPI_F = 1.144729886;
  CUDART_TWO_TO_M126_F = 1.175494351e-38;
  CUDART_TWO_TO_126_F = 8.507059173e37;
  CUDART_NORM_HUGE_F = 3.402823466e38;
  CUDART_TWO_TO_23_F = 8388608.0;
  CUDART_TWO_TO_24_F = 16777216.0;
  CUDART_TWO_TO_31_F = 2147483648.0;
  CUDART_TWO_TO_32_F = 4294967296.0;
  CUDART_REMQUO_BITS_F = 3;
  CUDART_REMQUO_MASK_F = CUDART_REMQUO_BITS_F;
  CUDART_TRIG_PLOSS_F = 48039.0;

  // double precision constants */
{$IFNDEF CUDA_NO_SM_13_DOUBLE_INTRINSICS}
  CUDART_INF: Double = $7FF0000000000000;
  CUDART_NAN: Double = $FFF8000000000000;
  CUDART_NEG_ZERO: Double = $8000000000000000;
  CUDART_MIN_DENORM: Double = $0000000000000001;
{$ELSE} // not CUDA_NO_SM_13_DOUBLE_INTRINSICS
  CUDART_INF: Double = $7FF0000000000000;
  CUDART_NAN: Double = $FFF8000000000000;
  CUDART_NEG_ZERO: Double = $8000000000000000;
  CUDART_MIN_DENORM: Double = $0000000000000001;
{$ENDIF}
  CUDART_ZERO = 0.0;
  CUDART_ONE = 1.0;
  CUDART_SQRT_TWO = 1.4142135623730951e+0;
  CUDART_SQRT_HALF = 7.0710678118654757e-1;
  CUDART_THIRD = 3.3333333333333333e-1;
  CUDART_TWOTHIRD = 6.6666666666666667e-1;
  CUDART_PIO4 = 7.8539816339744828e-1;
  CUDART_PIO4_HI = 7.8539816339744828e-1;
  CUDART_PIO4_LO = 3.0616169978683830e-17;
  CUDART_PIO2 = 1.5707963267948966e+0;
  CUDART_PIO2_HI = 1.5707963267948966e+0;
  CUDART_PIO2_LO = 6.1232339957367660e-17;
  CUDART_3PIO4 = 2.3561944901923448e+0;
  CUDART_2_OVER_PI = 6.3661977236758138e-1;
  CUDART_PI = 3.1415926535897931e+0;
  CUDART_PI_HI = 3.1415926535897931e+0;
  CUDART_PI_LO = 1.2246467991473532e-16;
  CUDART_SQRT_2PI_HI = 2.5066282746310007e+0;
  CUDART_SQRT_2PI_LO = -1.8328579980459167e-16;
  CUDART_SQRT_PIO2_HI = 1.2533141373155003e+0;
  CUDART_SQRT_PIO2_LO = -9.1642899902295834e-17;
  CUDART_L2E = 1.4426950408889634e+0;
  CUDART_L2E_HI = 1.4426950408889634e+0;
  CUDART_L2E_LO = 2.0355273740931033e-17;
  CUDART_L2T = 3.3219280948873622e+0;
  CUDART_LG2 = 3.0102999566398120e-1;
  CUDART_LG2_HI = 3.0102999566398120e-1;
  CUDART_LG2_LO = -2.8037281277851704e-18;
  CUDART_LGE = 4.3429448190325182e-1;
  CUDART_LGE_HI = 4.3429448190325182e-1;
  CUDART_LGE_LO = 1.09831965021676510e-17;
  CUDART_LN2 = 6.9314718055994529e-1;
  CUDART_LN2_HI = 6.9314718055994529e-1;
  CUDART_LN2_LO = 2.3190468138462996e-17;
  CUDART_LNT = 2.3025850929940459e+0;
  CUDART_LNT_HI = 2.3025850929940459e+0;
  CUDART_LNT_LO = -2.1707562233822494e-16;
  CUDART_LNPI = 1.1447298858494002e+0;
  CUDART_LN2_X_1024 = 7.0978271289338397e+2;
  CUDART_LN2_X_1025 = 7.1047586007394398e+2;
  CUDART_LN2_X_1075 = 7.4513321910194122e+2;
  CUDART_LG2_X_1024 = 3.0825471555991675e+2;
  CUDART_LG2_X_1075 = 3.2360724533877976e+2;
  CUDART_TWO_TO_23 = 8388608.0;
  CUDART_TWO_TO_52 = 4503599627370496.0;
  CUDART_TWO_TO_54 = 18014398509481984.0;
  CUDART_TWO_TO_M54 = 5.5511151231257827e-17;
  CUDART_TWO_TO_M1022 = 2.22507385850720140e-308;
  CUDART_TRIG_PLOSS = 2147483648.0;

type
  TcudaError = (
    cudaSuccess,
    cudaErrorMissingConfiguration,
    cudaErrorMemoryAllocation,
    cudaErrorInitializationError,
    cudaErrorLaunchFailure,
    cudaErrorPriorLaunchFailure,
    cudaErrorLaunchTimeout,
    cudaErrorLaunchOutOfResources,
    cudaErrorInvalidDeviceFunction,
    cudaErrorInvalidConfiguration,
    cudaErrorInvalidDevice,
    cudaErrorInvalidValue,
    cudaErrorInvalidPitchValue,
    cudaErrorInvalidSymbol,
    cudaErrorMapBufferObjectFailed,
    cudaErrorUnmapBufferObjectFailed,
    cudaErrorInvalidHostPointer,
    cudaErrorInvalidDevicePointer,
    cudaErrorInvalidTexture,
    cudaErrorInvalidTextureBinding,
    cudaErrorInvalidChannelDescriptor,
    cudaErrorInvalidMemcpyDirection,
    cudaErrorAddressOfConstant,
    cudaErrorTextureFetchFailed,
    cudaErrorTextureNotBound,
    cudaErrorSynchronizationError,
    cudaErrorInvalidFilterSetting,
    cudaErrorInvalidNormSetting,
    cudaErrorMixedDeviceExecution,
    cudaErrorCudartUnloading,
    cudaErrorUnknown,
    cudaErrorNotYetImplemented,
    cudaErrorMemoryValueTooLarge,
    cudaErrorInvalidResourceHandle,
    cudaErrorNotReady,
    cudaErrorStartupFailure,
    cudaErrorApiFailureBase);

  {+//DEVICE_BUILTIN*/ }
  TCudaChannelFormatKind = (
    cudaChannelFormatKindSigned,
    cudaChannelFormatKindUnsigned,
    cudaChannelFormatKindFloat);

  TCudaGLMapFlags = (
    cudaGLMapFlagsNone, ///< Default; Assume resource can be read/written
    cudaGLMapFlagsReadOnly, ///< CUDA kernels will not write to this resource
    cudaGLMapFlagsWriteDiscard);
  ///< CUDA kernels will only write to and will not read from this resource

{+//DEVICE_BUILTIN*/ }
  PcudaChannelFormatDesc = ^TCudaChannelFormatDesc;
  TCudaChannelFormatDesc = record
    x: Integer;
    y: Integer;
    z: Integer;
    w: Integer;
    f: TCudaChannelFormatKind;
  end {cudaChannelFormatDesc};

  {+//DEVICE_BUILTIN*/ }
  TcudaArray = record
  end; //!ATTENTION foreward Declaration?)

  {+//DEVICE_BUILTIN*/ }
  TcudaMemcpyKind = (
    cudaMemcpyHostToHost {= 0},
    cudaMemcpyHostToDevice,
    cudaMemcpyDeviceToHost,
    cudaMemcpyDeviceToDevice);

  {+//DEVICE_BUILTIN*/ }
  TcudaPitchedPtr = record
    ptr: Pointer;
    pitch: size_t;
    xsize: size_t;
    ysize: size_t;
  end {cudaPitchedPtr};

  {+//DEVICE_BUILTIN*/ }
  TcudaExtent = record
    width: size_t;
    height: size_t;
    depth: size_t;
  end {cudaExtent};

  {+//DEVICE_BUILTIN*/ }
  TcudaPos = record
    x: size_t;
    y: size_t;
    z: size_t;
  end {cudaPos};

  {+//DEVICE_BUILTIN*/ }
  TcudaMemcpy3DParms = record
    srcArray: Pointer;
    srcPos: TcudaPos;
    srcPtr: TcudaPitchedPtr;
    dstArray: Pointer;
    dstPos: TcudaPos;
    dstPtr: TcudaPitchedPtr;
    extent: TcudaExtent;
    kind: TcudaMemcpyKind;
  end {cudaMemcpy3DParms};

  {+//DEVICE_BUILTIN*/ }
  PCudaDeviceProp = ^TCudaDeviceProp;
  TCudaDeviceProp = record
    name: array[0..256 - 1] of AnsiChar;
    totalGlobalMem: size_t;
    sharedMemPerBlock: size_t;
    regsPerBlock: Integer;
    warpSize: Integer;
    memPitch: size_t;
    maxThreadsPerBlock: Integer;
    maxThreadsDim: array[0..3 - 1] of Integer;
    maxGridSize: array[0..3 - 1] of Integer;
    clockRate: Integer;
    totalConstMem: size_t;
    major: Integer;
    minor: Integer;
    textureAlignment: size_t;
    deviceOverlap: Integer;
    multiProcessorCount: Integer;
    __cudaReserved: array[0..40 - 1] of Integer;
  end {cudaDeviceProp};

  TcudaTextureAddressMode = (cudaAddressModeWrap, cudaAddressModeClamp);

  TcudaTextureFilterMode = (cudaFilterModePoint, cudaFilterModeLinear);

  TcudaTextureReadMode = (cudaReadModeElementType, cudaReadModeNormalizedFloat);

  PTextureReference = ^TTextureReference;
  TTextureReference = record
    normalized: Integer;
    filterMode: TcudaTextureFilterMode;
    addressMode: array[0..2] of TcudaTextureAddressMode;
    channelDesc: TcudaChannelFormatDesc;
    __cudaReserved: array[0..15] of Integer;
  end;

  PcudaArray = ^TcudaArray;

  {+//****************************************************************************** }
  {-** }
  {-* SHORTHAND TYPE DEFINITION USED BY RUNTIME API* }
  {-** }
  {=*******************************************************************************/ }
  {+//DEVICE_BUILTIN*/ }
  cudaError_t = TcudaError;
  {+//DEVICE_BUILTIN*/ }
  cudaStream_t = Integer;
  {+//DEVICE_BUILTIN*/ }
  cudaEvent_t = Integer;

  {+//****************************************************************************** }
  {-** }
  {-** }
  {-** }
  {=****************************************************************************** }

var

  cudaBindTexture: function(var offset: size_t; const texref: PTextureReference; var
    devPtr: Pointer; var desc: TcudaChannelFormatDesc; size: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cudaBindTexture2D: function(var offset: size_t; const texref: PTextureReference;
    const devPtr: Pointer; var desc: TcudaChannelFormatDesc; width, height, pitch:
    size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cudaBindTextureToArray: function(const texref: PTextureReference; const
    cudaArray: PcudaArray): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cudaUnbindTexture: function(const texref: PTextureReference): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cudaGetTextureAlignmentOffset: function(offset: size_t; const texref:
    PTextureReference): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cudaGetTextureReference: function(const texref: PTextureReference; const symbol:
    PAnsiChar): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGetChannelDesc: function(var desc: TcudaChannelFormatDesc; const array_: Pointer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaCreateChannelDesc: function(x, y, z, w: Integer; f: TCudaChannelFormatKind):
    TCudaChannelFormatDesc;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}


(*******************************************************************************
*                                                                              *
*                                                                              *
*                                                                              *
*******************************************************************************)

  cudaMalloc3D: function(var pitchedDevPtr: TcudaPitchedPtr; extent: TcudaExtent): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMalloc3DArray: function(var arrayPtr: PcudaArray; const desc: TcudaChannelFormatDesc; extent: TcudaExtent; flags: Cardinal): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemset3D: function(pitchedDevPtr: TcudaPitchedPtr; value: Integer; extent: TcudaExtent): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpy3D: function(const p: TcudaMemcpy3DParms): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpy3DAsync: function(const p: TcudaMemcpy3DParms; stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMalloc: function(var devPtr;
    size: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMallocHost: function(var ptr: Pointer;
    size: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMallocPitch: function(var devPtr;
    var pitch: size_t;
    width: size_t;
    height: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMallocArray: function(var aarray: Pointer;
    var desc: TCudaChannelFormatDesc;
    width: size_t;
    height: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaFree: function(devPtr: Pointer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaFreeHost: function(ptr: Pointer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaFreeArray: function(const aarray: Pointer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaHostAlloc: function(var pHost: Pointer; bytes: size_t; flags: Cardinal): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaHostGetDevicePointer: function(var pDevice: Pointer; pHost: Pointer; flags: Cardinal): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaHostGetFlags: function(var pFlags: Cardinal; pHost: Pointer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemGetInfo: function(var free: size_t; var total: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpy: function(dst: Pointer; src: Pointer; count: size_t;
    kind: TcudaMemcpyKind): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpyToArray: function(var dst: PcudaArray; wOffset: size_t; hOffset:
    size_t; var src; count: size_t; kind: TcudaMemcpyKind): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpyFromArray: function(var dst; const src: PcudaArray; wOffset: size_t;
    hOffset: size_t; count: size_t; kind: TcudaMemcpyKind): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpyArrayToArray: function(dst: PcudaArray; wOffsetDst: size_t; hOffsetDst:
    size_t; const src: PcudaArray; wOffsetSrc: size_t; hOffsetSrc: size_t; count:
    size_t; const kind: TcudaMemcpyKind = cudaMemcpyDeviceToDevice): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpy2D: function(var dst; dpitch: size_t; var src; spitch: size_t; width:
    size_t; height: size_t; kind: TcudaMemcpyKind): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpy2DToArray: function(dst: PcudaArray; wOffset: size_t; hOffset: size_t;
    var src; spitch: size_t; width: size_t; height: size_t; kind: TcudaMemcpyKind):
    cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpy2DFromArray: function(var dst; dpitch: size_t; src: PcudaArray;
    wOffset: size_t; hOffset: size_t; width: size_t; height: size_t; kind:
    TcudaMemcpyKind): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpy2DArrayToArray: function(dst: PcudaArray; wOffsetDst: size_t;
    hOffsetDst: size_t; src: PcudaArray; wOffsetSrc: size_t; hOffsetSrc: size_t;
    width: size_t; height: size_t; const kind: TcudaMemcpyKind =
    cudaMemcpyDeviceToDevice): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpyToSymbol: function(symbol: PAnsiChar; var src; count: size_t; const
    offset: size_t = 0; const kind: TcudaMemcpyKind = cudaMemcpyHostToDevice):
    cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpyFromSymbol: function(var dst; symbol: PAnsiChar; count: size_t; const
    offset: size_t = 0; const kind: TcudaMemcpyKind = cudaMemcpyDeviceToHost):
    cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  {+//***************************************************************************}
  {-** }
  {-** }
  {-** }
  {=*****************************************************************************}

  cudaMemcpyAsync: function(var dst; const src; count: size_t; kind:
    TcudaMemcpyKind; stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpyToArrayAsync: function(dst: PcudaArray; wOffset: size_t; hOffset:
    size_t; const src; count: size_t; kind: TcudaMemcpyKind; stream: cudaStream_t):
    cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpyFromArrayAsync: function(var dst; const src: PcudaArray; wOffset:
    size_t; hOffset: size_t; count: size_t; kind: TcudaMemcpyKind; stream:
    cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpy2DAsync: function(var dst; dpitch: size_t; const src; spitch: size_t;
    width: size_t; height: size_t; kind: TcudaMemcpyKind; stream: cudaStream_t):
    cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpy2DToArrayAsync: function(dst: PcudaArray; wOffset: size_t; hOffset:
    size_t; const src; spitch: size_t; width: size_t; height: size_t; kind:
    TcudaMemcpyKind; stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpy2DFromArrayAsync: function(var dst; dpitch: size_t; const src:
    PcudaArray; wOffset: size_t; hOffset: size_t; width: size_t; height: size_t;
    kind: TcudaMemcpyKind; stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpyToSymbolAsync: function(const symbol: PAnsiChar; const src; count:
    size_t; offset: size_t; kind: TcudaMemcpyKind; stream: cudaStream_t):
    cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemcpyFromSymbolAsync: function(var dst; const symbol: PAnsiChar; count:
    size_t; offset: size_t; kind: TcudaMemcpyKind; stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  ///*****************************************************************************
  //*                                                                            *
  //*                                                                            *
  //*                                                                            *
  //*****************************************************************************/

  cudaMemset: function(var devPtr; value: Integer; count: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaMemset2D: function(var devPtr; pitch: size_t; value: Integer; width: size_t;
    height: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  ///*****************************************************************************
  //*                                                                            *
  //*                                                                            *
  //*                                                                            *
  //*****************************************************************************/

  cudaGetSymbolAddress: function(var devPtr: Pointer; const symbol: PAnsiChar):
    cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGetSymbolSize: function(var size: size_t; const symbol: PAnsiChar):
    cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  {+//***************************************************************************}
  {-** }
  {-** }
  {-** }
  {=*****************************************************************************}

  cudaGetDeviceCount: function(var count: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGetDeviceProperties: function(var prop: TCudaDeviceProp;
    device: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaChooseDevice: function(var device: Integer;
    const prop: PcudaDeviceProp): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaSetDevice: function(device: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGetDevice: function(var device: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaSetDeviceFlags: function(flags: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaSetValidDevices: function(var device_arr: Integer; len: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  {+//****************************************************************************** }
  {-** }
  {-** }
  {-** }
  {=*******************************************************************************/ }

  cudaConfigureCall: function(gridDim, blockDim: TDim3; sharedMem: size_t; stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cudaSetupArgument: function(const arg: Pointer; size: size_t; offset: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cudaFuncSetCacheConfig: function(const func: PAnsiChar; cacheConfig: TcudaFuncCache): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cudaLaunch: function(const entry: PAnsiChar): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cudaFuncGetAttributes: function(var attr: TcudaFuncAttributes; const func: PAnsiChar): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  {+//****************************************************************************** }
  {-** }
  {-** }
  {-** }
  {=*******************************************************************************/ }

  cudaGetLastError: function: cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  {+//****************************************************************************** }
  {-** }
  {-** }
  {-** }
  {=*******************************************************************************/ }
  cudaGLSetGLDevice: function(device: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGLRegisterBufferObject: function(bufObj: GLuint): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cudaGraphicsGLRegisterImage: function(const resource: PCUgraphicsResource; image: GLuint; target: GLenum; Flags: Cardinal): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGraphicsGLRegisterBuffer: function(const resource: PCUgraphicsResource; buffer: GLuint; Flags: Cardinal): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGLMapBufferObject: function(devPtr: Pointer; bufObj: GLuint): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGLUnmapBufferObject: function(bufObj: GLuint): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGLUnregisterBufferObject: function(bufObj: GLuint): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGLSetBufferObjectMapFlags: function(bufObj: GLuint; flags: TCudaGLMapFlags):
    cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGLMapBufferObjectAsync: function(var devPtr: Pointer; bufObj: GLuint; stream:
    cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGLUnmapBufferObjectAsync: function(bufObj: GLuint; stream: cudaStream_t):
    cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

cudaGraphicsUnregisterResource: function(resource: PCUgraphicsResource): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

cudaGraphicsResourceSetMapFlags: function(resource: PCUgraphicsResource; flags: Cardinal): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

cudaGraphicsMapResources: function(count: Integer; const resources: PCUgraphicsResource; stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

cudaGraphicsUnmapResources: function(count: Integer; const resources: PCUgraphicsResource; stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

cudaGraphicsResourceGetMappedPointer: function(var pDevPtr: TCUdeviceptr; var pSize: Cardinal; resource: PCUgraphicsResource): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

cudaGraphicsSubResourceGetMappedArray: function(var pArray: PCUarray; resource:
    PCUgraphicsResource; arrayIndex: Cardinal; mipLevel: Cardinal): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaGetErrorString: function(error: cudaError_t): PAnsiChar;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaDriverGetVersion: function(var driverVersion: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaRuntimeGetVersion: function(var runtimeVersion: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

(*******************************************************************************
*                                                                              *
*                                                                              *
*                                                                              *
*******************************************************************************)

  cudaSetDoubleForDevice: function(var d: Double): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaSetDoubleForHost: function(var d: Double): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

(*******************************************************************************
*                                                                              *
*                                                                              *
*                                                                              *
*******************************************************************************)

  cudaStreamCreate: function(var pStream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaStreamDestroy: function(stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaStreamSynchronize: function(stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaStreamQuery: function(stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

(*******************************************************************************
*                                                                              *
*                                                                              *
*                                                                              *
*******************************************************************************)

  cudaEventCreate: function(var event: cudaEvent_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaEventCreateWithFlags: function(var event: cudaEvent_t; flags: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaEventRecord: function(event: cudaEvent_t; stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaEventQuery: function(event: cudaEvent_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaEventSynchronize: function(event: cudaEvent_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaEventDestroy: function(event: cudaEvent_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaEventElapsedTime: function(var ms: Single; start: cudaEvent_t; ending: cudaEvent_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaWGLGetDevice: function(var device: Integer; hGpu: HGPUNV): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

(*******************************************************************************
*                                                                              *
*                                                                              *
*                                                                              *
*******************************************************************************)

  cudaThreadExit: function(): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaThreadSynchronize: function(): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaThreadSetLimit: function(limit: TcudaLimit; value: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cudaThreadGetLimit: function(var Value: size_t; limit: TcudaLimit): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

function cudaGetLastErrorString: string;
function InitCUDART: Boolean;
procedure CloseCUDART;
function InitCUDARTFromLibrary(const LibName: WideString): Boolean;
function IsCUDARTInitialized: Boolean;

implementation

function cudaGetLastErrorString: string;
begin
  Result := string(cudaGetErrorString(cudaGetLastError));
end;

const
  INVALID_MODULEHANDLE = 0;

var
{$IFDEF MSWINDOWS}
  CUDARTHandle: HINST = INVALID_MODULEHANDLE;
{$ENDIF}

{$IFDEF LINUX}
  CUDARTHandle: TLibHandle = INVALID_MODULEHANDLE;
{$ENDIF}

function CUDARTGetProcAddress(ProcName: PAnsiChar): Pointer;
begin
  result := GetProcAddress(CUDARTHandle, ProcName);
end;

function InitCUDART: Boolean;
var
  I: Integer;
begin
  Result := True;
  if CUDARTHandle = INVALID_MODULEHANDLE then
  begin
    for I := 0 to High(CUDARTDLLNAMES) do
    begin
      if InitCUDARTFromLibrary(CUDARTDLLNAMES[I] + '.dll') then
        Exit;
    end;
  end;
  Result := False;
end;

procedure CloseCUDART;
begin
  if CUDARTHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(CUDARTHandle);
    CUDARTHandle := INVALID_MODULEHANDLE;
  end;
end;

function InitCUDARTFromLibrary(const LibName: WideString): Boolean;
begin
  Result := False;
  CloseCUDART;
  CUDARTHandle := LoadLibraryW(PWideChar(LibName));
  if CUDARTHandle = INVALID_MODULEHANDLE then
    Exit;
  cudaFreeHost := CUDARTGetProcAddress('cudaFreeHost');
  cudaFuncGetAttributes := CUDARTGetProcAddress('cudaFuncGetAttributes');
  cudaGetChannelDesc := CUDARTGetProcAddress('cudaGetChannelDesc');
  cudaGetDevice := CUDARTGetProcAddress('cudaGetDevice');
  cudaGetDeviceCount := CUDARTGetProcAddress('cudaGetDeviceCount');
  cudaGetDeviceProperties := CUDARTGetProcAddress('cudaGetDeviceProperties');
  cudaGetErrorString := CUDARTGetProcAddress('cudaGetErrorString');
  cudaGetLastError := CUDARTGetProcAddress('cudaGetLastError');
  cudaGetSymbolAddress := CUDARTGetProcAddress('cudaGetSymbolAddress');
  cudaGetSymbolSize := CUDARTGetProcAddress('cudaGetSymbolSize');
  cudaGetTextureAlignmentOffset := CUDARTGetProcAddress('cudaGetTextureAlignmentOffset');
  cudaGetTextureReference := CUDARTGetProcAddress('cudaGetTextureReference');
  cudaGLMapBufferObject := CUDARTGetProcAddress('cudaGLMapBufferObject');
  cudaGLMapBufferObjectAsync := CUDARTGetProcAddress('cudaGLMapBufferObjectAsync');
  cudaGLRegisterBufferObject := CUDARTGetProcAddress('cudaGLRegisterBufferObject');
  cudaGLSetBufferObjectMapFlags := CUDARTGetProcAddress('cudaGLSetBufferObjectMapFlags');
  cudaGLSetGLDevice := CUDARTGetProcAddress('cudaGLSetGLDevice');
  cudaGLUnmapBufferObject := CUDARTGetProcAddress('cudaGLUnmapBufferObject');
  cudaGLUnmapBufferObjectAsync := CUDARTGetProcAddress('cudaGLUnmapBufferObjectAsync');
  cudaGLUnregisterBufferObject := CUDARTGetProcAddress('cudaGLUnregisterBufferObject');
  cudaGraphicsGLRegisterBuffer := CUDARTGetProcAddress('cudaGraphicsGLRegisterBuffer');
  cudaGraphicsGLRegisterImage := CUDARTGetProcAddress('cudaGraphicsGLRegisterImage');
  cudaGraphicsMapResources := CUDARTGetProcAddress('cudaGraphicsMapResources');
  cudaGraphicsResourceGetMappedPointer := CUDARTGetProcAddress('cudaGraphicsResourceGetMappedPointer');
  cudaGraphicsResourceSetMapFlags := CUDARTGetProcAddress('cudaGraphicsResourceSetMapFlags');
  cudaGraphicsSubResourceGetMappedArray := CUDARTGetProcAddress('cudaGraphicsSubResourceGetMappedArray');
  cudaGraphicsUnmapResources := CUDARTGetProcAddress('cudaGraphicsUnmapResources');
  cudaGraphicsUnregisterResource := CUDARTGetProcAddress('cudaGraphicsUnregisterResource');
  cudaHostAlloc := CUDARTGetProcAddress('cudaHostAlloc');
  cudaHostGetDevicePointer := CUDARTGetProcAddress('cudaHostGetDevicePointer');
  cudaHostGetFlags := CUDARTGetProcAddress('cudaHostGetFlags');
  cudaLaunch := CUDARTGetProcAddress('cudaLaunch');
  cudaMalloc := CUDARTGetProcAddress('cudaMalloc');
  cudaMalloc3D := CUDARTGetProcAddress('cudaMalloc3D');
  cudaMalloc3DArray := CUDARTGetProcAddress('cudaMalloc3DArray');
  cudaMallocArray := CUDARTGetProcAddress('cudaMallocArray');
  cudaMallocHost := CUDARTGetProcAddress('cudaMallocHost');
  cudaMallocPitch := CUDARTGetProcAddress('cudaMallocPitch');
  cudaMemcpy := CUDARTGetProcAddress('cudaMemcpy');
  cudaMemcpy2D := CUDARTGetProcAddress('cudaMemcpy2D');
  cudaMemcpy2DArrayToArray := CUDARTGetProcAddress('cudaMemcpy2DArrayToArray');
  cudaMemcpy2DAsync := CUDARTGetProcAddress('cudaMemcpy2DAsync');
  cudaMemcpy2DFromArray := CUDARTGetProcAddress('cudaMemcpy2DFromArray');
  cudaMemcpy2DFromArrayAsync := CUDARTGetProcAddress('cudaMemcpy2DFromArrayAsync');
  cudaMemcpy2DToArray := CUDARTGetProcAddress('cudaMemcpy2DToArray');
  cudaMemcpy2DToArrayAsync := CUDARTGetProcAddress('cudaMemcpy2DToArrayAsync');
  cudaMemcpy3D := CUDARTGetProcAddress('cudaMemcpy3D');
  cudaMemcpy3DAsync := CUDARTGetProcAddress('cudaMemcpy3DAsync');
  cudaMemcpyArrayToArray := CUDARTGetProcAddress('cudaMemcpyArrayToArray');
  cudaMemcpyAsync := CUDARTGetProcAddress('cudaMemcpyAsync');
  cudaMemcpyFromArray := CUDARTGetProcAddress('cudaMemcpyFromArray');
  cudaMemcpyFromArrayAsync := CUDARTGetProcAddress('cudaMemcpyFromArrayAsync');
  cudaMemcpyFromSymbol := CUDARTGetProcAddress('cudaMemcpyFromSymbol');
  cudaMemcpyFromSymbolAsync := CUDARTGetProcAddress('cudaMemcpyFromSymbolAsync');
  cudaMemcpyToArray := CUDARTGetProcAddress('cudaMemcpyToArray');
  cudaMemcpyToArrayAsync := CUDARTGetProcAddress('cudaMemcpyToArrayAsync');
  cudaMemcpyToSymbol := CUDARTGetProcAddress('cudaMemcpyToSymbol');
  cudaMemcpyToSymbolAsync := CUDARTGetProcAddress('cudaMemcpyToSymbolAsync');
  cudaMemGetInfo := CUDARTGetProcAddress('cudaMemGetInfo');
  cudaMemset := CUDARTGetProcAddress('cudaMemset');
  cudaMemset2D := CUDARTGetProcAddress('cudaMemset2D');
  cudaMemset3D := CUDARTGetProcAddress('cudaMemset3D');
  cudaRuntimeGetVersion := CUDARTGetProcAddress('cudaRuntimeGetVersion');
  cudaSetDevice := CUDARTGetProcAddress('cudaSetDevice');
  cudaSetDeviceFlags := CUDARTGetProcAddress('cudaSetDeviceFlags');
  cudaSetDoubleForDevice := CUDARTGetProcAddress('cudaSetDoubleForDevice');
  cudaSetDoubleForHost := CUDARTGetProcAddress('cudaSetDoubleForHost');
  cudaSetupArgument := CUDARTGetProcAddress('cudaSetupArgument');
  cudaSetValidDevices := CUDARTGetProcAddress('cudaSetValidDevices');
  cudaStreamCreate := CUDARTGetProcAddress('cudaStreamCreate');
  cudaStreamDestroy := CUDARTGetProcAddress('cudaStreamDestroy');
  cudaStreamQuery := CUDARTGetProcAddress('cudaStreamQuery');
  cudaStreamSynchronize := CUDARTGetProcAddress('cudaStreamSynchronize');
  cudaThreadExit := CUDARTGetProcAddress('cudaThreadExit');
  cudaThreadSynchronize := CUDARTGetProcAddress('cudaThreadSynchronize');
  cudaThreadSetLimit := CUDARTGetProcAddress('cudaThreadSetLimit');
  cudaThreadGetLimit := CUDARTGetProcAddress('cudaThreadGetLimit');
  cudaUnbindTexture := CUDARTGetProcAddress('cudaUnbindTexture');
  cudaWGLGetDevice := CUDARTGetProcAddress('cudaWGLGetDevice');

  Result := True;
end;

function IsCUDARTInitialized: Boolean;
begin
  Result := (CUDARTHandle <> INVALID_MODULEHANDLE);
end;

initialization

finalization

  CloseCUDART;

end.

