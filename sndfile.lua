---
--- @classmod sndfile
---

-- module table
local sndfile = {}

local bit = require "bit"
local ffi = require "ffi"

ffi.cdef[[
typedef struct SNDFILE_tag SNDFILE;

typedef int64_t sf_count_t;

typedef enum SF_FORMAT {	/* Major formats. */
	SF_FORMAT_WAV			= 0x010000,		/* Microsoft WAV format (little endian default). */
	SF_FORMAT_AIFF			= 0x020000,		/* Apple/SGI AIFF format (big endian). */
	SF_FORMAT_AU			= 0x030000,		/* Sun/NeXT AU format (big endian). */
	SF_FORMAT_RAW			= 0x040000,		/* RAW PCM data. */
	SF_FORMAT_PAF			= 0x050000,		/* Ensoniq PARIS file format. */
	SF_FORMAT_SVX			= 0x060000,		/* Amiga IFF / SVX8 / SV16 format. */
	SF_FORMAT_NIST			= 0x070000,		/* Sphere NIST format. */
	SF_FORMAT_VOC			= 0x080000,		/* VOC files. */
	SF_FORMAT_IRCAM			= 0x0A0000,		/* Berkeley/IRCAM/CARL */
	SF_FORMAT_W64			= 0x0B0000,		/* Sonic Foundry's 64 bit RIFF/WAV */
	SF_FORMAT_MAT4			= 0x0C0000,		/* Matlab (tm) V4.2 / GNU Octave 2.0 */
	SF_FORMAT_MAT5			= 0x0D0000,		/* Matlab (tm) V5.0 / GNU Octave 2.1 */
	SF_FORMAT_PVF			= 0x0E0000,		/* Portable Voice Format */
	SF_FORMAT_XI			= 0x0F0000,		/* Fasttracker 2 Extended Instrument */
	SF_FORMAT_HTK			= 0x100000,		/* HMM Tool Kit format */
	SF_FORMAT_SDS			= 0x110000,		/* Midi Sample Dump Standard */
	SF_FORMAT_AVR			= 0x120000,		/* Audio Visual Research */
	SF_FORMAT_WAVEX			= 0x130000,		/* MS WAVE with WAVEFORMATEX */
	SF_FORMAT_SD2			= 0x160000,		/* Sound Designer 2 */
	SF_FORMAT_FLAC			= 0x170000,		/* FLAC lossless file format */
	SF_FORMAT_CAF			= 0x180000,		/* Core Audio File format */
	SF_FORMAT_WVE			= 0x190000,		/* Psion WVE format */
	SF_FORMAT_OGG			= 0x200000,		/* Xiph OGG container */
	SF_FORMAT_MPC2K			= 0x210000,		/* Akai MPC 2000 sampler */
	SF_FORMAT_RF64			= 0x220000,		/* RF64 WAV file */
	SF_FORMAT_MPEG			= 0x230000,		/* MPEG-1/2 audio stream */

	/* Subtypes from here on. */

	SF_FORMAT_PCM_S8		= 0x0001,		/* Signed 8 bit data */
	SF_FORMAT_PCM_16		= 0x0002,		/* Signed 16 bit data */
	SF_FORMAT_PCM_24		= 0x0003,		/* Signed 24 bit data */
	SF_FORMAT_PCM_32		= 0x0004,		/* Signed 32 bit data */

	SF_FORMAT_PCM_U8		= 0x0005,		/* Unsigned 8 bit data (WAV and RAW only) */

	SF_FORMAT_FLOAT			= 0x0006,		/* 32 bit float data */
	SF_FORMAT_DOUBLE		= 0x0007,		/* 64 bit float data */

	SF_FORMAT_ULAW			= 0x0010,		/* U-Law encoded. */
	SF_FORMAT_ALAW			= 0x0011,		/* A-Law encoded. */
	SF_FORMAT_IMA_ADPCM		= 0x0012,		/* IMA ADPCM. */
	SF_FORMAT_MS_ADPCM		= 0x0013,		/* Microsoft ADPCM. */

	SF_FORMAT_GSM610		= 0x0020,		/* GSM 6.10 encoding. */
	SF_FORMAT_VOX_ADPCM		= 0x0021,		/* OKI / Dialogix ADPCM */

	SF_FORMAT_NMS_ADPCM_16	= 0x0022,		/* 16kbs NMS G721-variant encoding. */
	SF_FORMAT_NMS_ADPCM_24	= 0x0023,		/* 24kbs NMS G721-variant encoding. */
	SF_FORMAT_NMS_ADPCM_32	= 0x0024,		/* 32kbs NMS G721-variant encoding. */

	SF_FORMAT_G721_32		= 0x0030,		/* 32kbs G721 ADPCM encoding. */
	SF_FORMAT_G723_24		= 0x0031,		/* 24kbs G723 ADPCM encoding. */
	SF_FORMAT_G723_40		= 0x0032,		/* 40kbs G723 ADPCM encoding. */

	SF_FORMAT_DWVW_12		= 0x0040, 		/* 12 bit Delta Width Variable Word encoding. */
	SF_FORMAT_DWVW_16		= 0x0041, 		/* 16 bit Delta Width Variable Word encoding. */
	SF_FORMAT_DWVW_24		= 0x0042, 		/* 24 bit Delta Width Variable Word encoding. */
	SF_FORMAT_DWVW_N		= 0x0043, 		/* N bit Delta Width Variable Word encoding. */

	SF_FORMAT_DPCM_8		= 0x0050,		/* 8 bit differential PCM (XI only) */
	SF_FORMAT_DPCM_16		= 0x0051,		/* 16 bit differential PCM (XI only) */

	SF_FORMAT_VORBIS		= 0x0060,		/* Xiph Vorbis encoding. */
	SF_FORMAT_OPUS			= 0x0064,		/* Xiph/Skype Opus encoding. */

	SF_FORMAT_ALAC_16		= 0x0070,		/* Apple Lossless Audio Codec (16 bit). */
	SF_FORMAT_ALAC_20		= 0x0071,		/* Apple Lossless Audio Codec (20 bit). */
	SF_FORMAT_ALAC_24		= 0x0072,		/* Apple Lossless Audio Codec (24 bit). */
	SF_FORMAT_ALAC_32		= 0x0073,		/* Apple Lossless Audio Codec (32 bit). */

	SF_FORMAT_MPEG_LAYER_I	= 0x0080,		/* MPEG-1 Audio Layer I */
	SF_FORMAT_MPEG_LAYER_II	= 0x0081,		/* MPEG-1 Audio Layer II */
	SF_FORMAT_MPEG_LAYER_III = 0x0082,		/* MPEG-2 Audio Layer III */

	/* Endian-ness options. */

	SF_ENDIAN_FILE			= 0x00000000,	/* Default file endian-ness. */
	SF_ENDIAN_LITTLE		= 0x10000000,	/* Force little endian-ness. */
	SF_ENDIAN_BIG			= 0x20000000,	/* Force big endian-ness. */
	SF_ENDIAN_CPU			= 0x30000000,	/* Force CPU endian-ness. */

	SF_FORMAT_SUBMASK		= 0x0000FFFF,
	SF_FORMAT_TYPEMASK		= 0x0FFF0000,
	SF_FORMAT_ENDMASK		= 0x30000000
} SF_FORMAT;

typedef struct SF_INFO {
	sf_count_t      frames;                /* Used to be called samples.  Changed to avoid confusion. */
	int             samplerate;
	int             channels;
	int             format;
	int             sections;
	int             seekable;
} SF_INFO;

typedef struct {
	int             format;
	const char      *name;
	const char      *extension;
} SF_FORMAT_INFO;

/* The enum was not named in the sndfile.h */
typedef enum SF_MODE {
        /* Modes for opening files. */
        SFM_READ        = 0x10,
        SFM_WRITE       = 0x20,
        SFM_RDWR        = 0x30
} SF_MODE;

/* These values come from stdio.h */
typedef enum SF_SEEK {
	SEEK_SET = 0,
	SEEK_CUR = 1,
	SEEK_END = 2
} SF_SEEK;

SNDFILE* sf_open(const char *path, int mode, SF_INFO *sfinfo);

sf_count_t sf_seek(SNDFILE *sndfile, sf_count_t frames, int whence);

const char* sf_strerror(SNDFILE *sndfile);

int sf_command(SNDFILE *sndfile, int command, void *data, int datasize);

sf_count_t sf_read_double(SNDFILE *sndfile, double *ptr, sf_count_t items);
sf_count_t sf_readf_double(SNDFILE *sndfile, double *ptr, sf_count_t frames);

sf_count_t sf_write_double(SNDFILE *sndfile, const double *ptr, sf_count_t items);
sf_count_t sf_writef_double(SNDFILE *sndfile, const double *ptr, sf_count_t frames);

int sf_close(SNDFILE *sndfile);
]]

local lib = ffi.load("sndfile")

sndfile.frame_type = ffi.typeof("double[?]")

-- This can be reused in sndfile:read() and sndfile:write()
-- to avoid allocations.
local double_buffer = sndfile.frame_type(1)

-- NOTE: Constants are also in ffi.C
sndfile.SF_FORMAT = ffi.typeof("SF_FORMAT")

-- FIXME: Maybe fall back to sf_command(SFC_GET_SIMPLE_FORMAT)
-- for unknown extensions. The simple formats are not always
-- ideal. 
function sndfile.guess_format(filename)
	local ext2format = {
		raw = ffi.C.SF_FORMAT_RAW + ffi.C.SF_FORMAT_DOUBLE,
		wav = ffi.C.SF_FORMAT_WAV + ffi.C.SF_FORMAT_FLOAT,
		xi = ffi.C.SF_FORMAT_XI + ffi.C.SF_FORMAT_DPCM_16,
		flac = ffi.C.SF_FORMAT_FLAC + ffi.C.SF_FORMAT_PCM_24,
		ogg = ffi.C.SF_FORMAT_OGG + ffi.C.SF_FORMAT_DOUBLE
	}

	local ext = filename:match("%.(.+)$")

	-- assume raw format for missing extensions
	local format = ext2format[ext and ext:lower() or "raw"]
	if not format then error("Unknown extension \""..ext.."\"") end

	return format
end

function sndfile:new(path, mode, samplerate, channels, format)
	local obj = {}

	local info = ffi.new("SF_INFO[1]")

	if mode == "SFM_WRITE" or
	   (format and bit.band(format, ffi.C.SF_FORMAT_TYPEMASK) == ffi.C.SF_FORMAT_RAW) then
		info[0].samplerate = samplerate or 44100
		info[0].channels = channels or 1
		info[0].format = format or sndfile.guess_format(path)
	end

	obj.handle = lib.sf_open(path, ffi.new("SF_MODE", mode), info)
	if obj.handle == nil then
		error(ffi.string(lib.sf_strerror(nil)))
	end

	obj.handle = ffi.gc(obj.handle, lib.sf_close)

	obj.info = info[0]

	return setmetatable(obj, {__index = self})
end

function sndfile:seek(frames, whence)
	whence = whence and ffi.new("SF_SEEK", whence) or ffi.C.SEEK_SET
	return lib.sf_seek(self.handle, frames, whence)
end

-- TODO: Maybe support reading multiple samples at once.
function sndfile:read()
	return lib.sf_read_double(self.handle, double_buffer, 1) == 1 and
	       tonumber(double_buffer[0]) or nil
end

function sndfile:readf(frame)
	return lib.sf_readf_double(self.handle, frame, 1) == 1
end

-- TODO: Maybe support writing multiple samples at once
function sndfile:write(sample)
	double_buffer[0] = sample
	return lib.sf_write_double(self.handle, double_buffer, 1)
end

function sndfile:writef(frame)
	return lib.sf_writef_double(self.handle, frame, 1)
end

function sndfile:close()
	if self.handle then
		-- NOTE: Finalizer must be removed to avoid a
		-- double-close here and later by the garbage collector.
		lib.sf_close(ffi.gc(self.handle, nil))
		self.handle = nil
	end
end

return sndfile
