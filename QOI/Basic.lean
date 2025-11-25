-- QOI (Quite OK Image) Format Implementation
-- Specification: https://qoiformat.org/

-- TODO: Add intermediary step for compressed image representation (list of chunks, not bytes)
namespace QOI.Basic

structure RGBA where
  r : UInt8
  g : UInt8
  b : UInt8
  a : UInt8
  deriving Repr, BEq, Inhabited

structure QOIHeader where
  width : UInt32
  height : UInt32
  channels : UInt8  -- 3 = RGB, 4 = RGBA
  colorspace : UInt8  -- 0 = sRGB with linear alpha, 1 = all channels linear
  deriving Repr

def QOI_MAGIC : UInt32 := 0x716f6966  -- "qoif" in ASCII
def QOI_END_MARKER : List UInt8 := [0, 0, 0, 0, 0, 0, 0, 1]


def QOI_OP_RGB   : UInt8 := 0xfe
structure QOIChunkRGB where
  r : UInt8
  g : UInt8
  b : UInt8
  deriving Repr, BEq

def QOI_OP_RGBA  : UInt8 := 0xff
structure QOIChunkRGBA where
  r : UInt8
  g : UInt8
  b : UInt8
  a : UInt8
  deriving Repr, BEq

def QOI_OP_INDEX : UInt8 := 0x00  -- 00xxxxxx
structure QOIChunkIndex where
  index : UInt8  -- 6-bit value (0-63)
  deriving Repr, BEq

def QOI_OP_DIFF  : UInt8 := 0x40  -- 01xxxxxx
structure QOIChunkDiff where
  dr : Int8  -- -2 to 1
  dg : Int8  -- -2 to 1
  db : Int8  -- -2 to 1
  deriving Repr, BEq

def QOI_OP_LUMA  : UInt8 := 0x80  -- 10xxxxxx
structure QOIChunkLuma where
  dg : Int8      -- -32 to 31
  dr_dg : Int8   -- dr - dg, range -8 to 7
  db_dg : Int8   -- db - dg, range -8 to 7
  deriving Repr, BEq

def QOI_OP_RUN   : UInt8 := 0xc0  -- 11xxxxxx
structure QOIChunkRun where
  runLength : Nat  -- 1 to 62
  deriving Repr, BEq

-- Sum type for all chunk types
inductive QOIChunk where
  | rgb : QOIChunkRGB → QOIChunk
  | rgba : QOIChunkRGBA → QOIChunk
  | index : QOIChunkIndex → QOIChunk
  | diff : QOIChunkDiff → QOIChunk
  | luma : QOIChunkLuma → QOIChunk
  | run : QOIChunkRun → QOIChunk
  deriving Repr

def encodeChunk (chunk : QOIChunk) : List UInt8 :=
  match chunk with
  | .rgb c => [QOI_OP_RGB, c.r, c.g, c.b]
  | .rgba c => [QOI_OP_RGBA, c.r, c.g, c.b, c.a]
  | .index c => [QOI_OP_INDEX ||| (c.index &&& 0x3f)]
  | .diff c =>
    let tag := QOI_OP_DIFF |||
               ((c.dr + 2).toUInt8 <<< 4) |||
               ((c.dg + 2).toUInt8 <<< 2) |||
               (c.db + 2).toUInt8
    [tag]
  | .luma c =>
    let byte1 := QOI_OP_LUMA ||| (c.dg + 32).toUInt8
    let byte2 := ((c.dr_dg + 8).toUInt8 <<< 4) ||| (c.db_dg + 8).toUInt8
    [byte1, byte2]
  | .run c =>
    let tag := QOI_OP_RUN ||| ((c.runLength - 1).toUInt8 &&& 0x3f)
    [tag]

-- Decode bytes to chunk and return (chunk, bytes consumed)
def decodeChunk (bytes : List UInt8) (pos : Nat) : Option (QOIChunk × Nat) := do
  let byte ← bytes[pos]?

  if byte == QOI_OP_RGB then
    if pos + 3 >= bytes.length then none
    else
      let r ← bytes[pos + 1]?
      let g ← bytes[pos + 2]?
      let b ← bytes[pos + 3]?
      some (QOIChunk.rgb { r, g, b }, 4)

  else if byte == QOI_OP_RGBA then
    if pos + 4 >= bytes.length then none
    else
      let r ← bytes[pos + 1]?
      let g ← bytes[pos + 2]?
      let b ← bytes[pos + 3]?
      let a ← bytes[pos + 4]?
      some (QOIChunk.rgba { r, g, b, a }, 5)

  else if (byte &&& 0xc0) == QOI_OP_INDEX then
    let index := byte &&& 0x3f
    some (QOIChunk.index { index }, 1)

  else if (byte &&& 0xc0) == QOI_OP_DIFF then
    let dr := ((byte >>> 4) &&& 0x03).toInt8 - 2
    let dg := ((byte >>> 2) &&& 0x03).toInt8 - 2
    let db := (byte &&& 0x03).toInt8 - 2
    some (QOIChunk.diff { dr, dg, db }, 1)

  else if (byte &&& 0xc0) == QOI_OP_LUMA then
    if pos + 1 >= bytes.length then none
    else
      let byte2 ← bytes[pos + 1]?
      let dg := (byte &&& 0x3f).toInt8 - 32
      let dr_dg := ((byte2 >>> 4) &&& 0x0f).toInt8 - 8
      let db_dg := (byte2 &&& 0x0f).toInt8 - 8
      some (QOIChunk.luma { dg, dr_dg, db_dg }, 2)

  else if (byte &&& 0xc0) == QOI_OP_RUN then
    let runLength := (byte &&& 0x3f).toNat + 1
    some (QOIChunk.run { runLength }, 1)

  else
    none

-- Apply chunk to get new pixel
def applyChunk (chunk : QOIChunk) (prevPixel : RGBA) (index : Array RGBA) : RGBA :=
  match chunk with
  | .rgb c => { r := c.r, g := c.g, b := c.b, a := prevPixel.a }
  | .rgba c => { r := c.r, g := c.g, b := c.b, a := c.a }
  | .index c => index[c.index.toNat]!
  | .diff c =>
    { r := (prevPixel.r.toInt8 + c.dr).toUInt8
    , g := (prevPixel.g.toInt8 + c.dg).toUInt8
    , b := (prevPixel.b.toInt8 + c.db).toUInt8
    , a := prevPixel.a
    }
  | .luma c =>
    let dr := c.dr_dg + c.dg
    let db := c.db_dg + c.dg
    { r := (prevPixel.r.toInt8 + dr).toUInt8
    , g := (prevPixel.g.toInt8 + c.dg).toUInt8
    , b := (prevPixel.b.toInt8 + db).toUInt8
    , a := prevPixel.a
    }
  | .run _ => prevPixel

/-! ## Utility Functions -/

-- Hash function for color indexing
def hashColor (px : RGBA) : UInt8 :=
  ((px.r.toNat * 3 + px.g.toNat * 5 + px.b.toNat * 7 + px.a.toNat * 11) % 64).toUInt8

-- Helper functions for byte operations
def uint32ToBytes (n : UInt32) : List UInt8 :=
  [ (n >>> 24).toUInt8
  , (n >>> 16).toUInt8
  , (n >>> 8).toUInt8
  , n.toUInt8
  ]

def bytesToUInt32 (bytes : List UInt8) : Option UInt32 :=
  match bytes with
  | [b1, b2, b3, b4] =>
    some (b1.toUInt32 <<< 24 ||| b2.toUInt32 <<< 16 ||| b3.toUInt32 <<< 8 ||| b4.toUInt32)
  | _ => none

def encodeHeader (header : QOIHeader) : List UInt8 :=
  uint32ToBytes QOI_MAGIC ++
  uint32ToBytes header.width ++
  uint32ToBytes header.height ++
  [header.channels, header.colorspace]

def decodeHeader (bytes : List UInt8) : Option (QOIHeader × List UInt8) := do
  guard (bytes.length >= 14)

  let magic ← bytesToUInt32 (bytes.take 4)
  guard (magic == QOI_MAGIC)

  let width ← bytesToUInt32 ((bytes.drop 4).take 4)
  let height ← bytesToUInt32 ((bytes.drop 8).take 4)
  let channels ← bytes[12]?
  let colorspace ← bytes[13]?

  guard (channels == 3 || channels == 4)
  guard (colorspace <= 1)

  some ({ width, height, channels, colorspace }, bytes.drop 14)

/-! ## Unified State for Encoding/Decoding -/

-- Unified state used by both encoder and decoder
structure QOIState where
  prevPixel : RGBA
  index : Array RGBA  -- 64 elements for color palette
  deriving Inhabited

def initQOIState : QOIState :=
  { prevPixel := { r := 0, g := 0, b := 0, a := 255 }
  , index := Array.replicate 64 { r := 0, g := 0, b := 0, a := 0 }
  }

/-! ## Encoder Functions -/

def MAX_RUN_LENGTH : Nat := 62

-- Helper: Emit a chunk and update state, returns (newState, encodedBytes)
def emitChunk (state : QOIState) (chunk : QOIChunk) (px : RGBA) : QOIState × List UInt8 :=
  let hashIdx := hashColor px
  let newState := { state with
    prevPixel := px
    index := state.index.set! hashIdx.toNat px
  }
  (newState, encodeChunk chunk)

-- Helper: Encode run-length chunk
def encodeRun (runLength : Nat) : List UInt8 :=
  if runLength > 0 then
    let chunk := QOIChunk.run { runLength := runLength }
    encodeChunk chunk
  else
    []

-- Helper: Check if differences fit in DIFF chunk range
def inDiffRange (dr dg db : Int8) : Bool :=
  dr >= -2 && dr <= 1 && dg >= -2 && dg <= 1 && db >= -2 && db <= 1

-- Helper: Check if differences fit in LUMA chunk range
def inLumaRange (dr dg db : Int8) : Bool :=
  dg >= -32 && dg <= 31 && (dr - dg) >= -8 && (dr - dg) <= 7 && (db - dg) >= -8 && (db - dg) <= 7

-- Helper: Choose best chunk for encoding a pixel
def chooseChunk (px : RGBA) (prev : RGBA) : QOIChunk :=
  let dr := px.r.toInt8 - prev.r.toInt8
  let dg := px.g.toInt8 - prev.g.toInt8
  let db := px.b.toInt8 - prev.b.toInt8
  let da := px.a.toInt8 - prev.a.toInt8

  if da != 0 then
    -- Alpha changed - must use RGBA
    QOIChunk.rgba { r := px.r, g := px.g, b := px.b, a := px.a }
  else if inDiffRange dr dg db then
    -- Small differences - use DIFF
    QOIChunk.diff { dr, dg, db }
  else if inLumaRange dr dg db then
    -- Medium differences - use LUMA
    QOIChunk.luma { dg, dr_dg := dr - dg, db_dg := db - dg }
  else
    -- Large differences - use RGB
    QOIChunk.rgb { r := px.r, g := px.g, b := px.b }

-- Encode a single pixel (returns state, output bytes, and run-length)
def encodePixel (state : QOIState) (px : RGBA) (runLength : Nat) : QOIState × List UInt8 × Nat :=
  if px == state.prevPixel then
    -- Same pixel - extend run
    let newRunLength := runLength + 1
    if newRunLength == MAX_RUN_LENGTH then
      -- Flush the run at max length
      let runBytes := encodeRun newRunLength
      let updatedState := { state with prevPixel := px }
      (updatedState, runBytes, 0)
    else
      -- Continue accumulating run
      (state, [], newRunLength)
  else
    -- Different pixel - flush run and encode
    let runBytes := encodeRun runLength
    let hashIdx := hashColor px

    if state.index[hashIdx.toNat]! == px then
      -- Pixel found in index
      let (newState, chunkBytes) := emitChunk state (QOIChunk.index { index := hashIdx }) px
      (newState, runBytes ++ chunkBytes, 0)
    else
      -- Choose best encoding method
      let (newState, chunkBytes) := emitChunk state (chooseChunk px state.prevPixel) px
      (newState, runBytes ++ chunkBytes, 0)

-- Helper for folding over pixels with run-length tracking
def encodePixelWithState (acc : QOIState × List UInt8 × Nat) (px : RGBA) : QOIState × List UInt8 × Nat :=
  let (state, output, runLength) := acc
  let (newState, newBytes, newRunLength) := encodePixel state px runLength
  (newState, output ++ newBytes, newRunLength)

-- Encode pixel data
def encodePixels (pixels : List RGBA) : List UInt8 :=
  let (_, output, runLength) := pixels.foldl encodePixelWithState (initQOIState, [], 0)
  let finalRunBytes := encodeRun runLength
  output ++ finalRunBytes ++ QOI_END_MARKER

/-! ## Main Encode Function -/

-- Main encode function
def encode (width : UInt32) (height : UInt32) (channels : UInt8) (pixels : List RGBA) : Option (List UInt8) := do
  -- Validate inputs
  guard (channels == 3 || channels == 4)
  guard (pixels.length == width.toNat * height.toNat)

  -- Encode header and pixels
  let header : QOIHeader := { width, height, channels, colorspace := 0 }
  some (encodeHeader header ++ encodePixels pixels)

/-! ## Decoder Functions -/

-- Helper: Replicate a pixel n times
def replicatePixel (px : RGBA) : Nat → List RGBA → List RGBA
  | 0, acc => acc
  | n + 1, acc => replicatePixel px n (px :: acc)

-- Helper: Process a decoded chunk and update state, returns (newState, newPixels)
def processChunk (state : QOIState) (chunk : QOIChunk) : QOIState × List RGBA :=
  match chunk with
  | .run runChunk =>
    -- Run-length encoding: repeat previous pixel
    (state, replicatePixel state.prevPixel runChunk.runLength [])
  | _ =>
    -- Compute new pixel from chunk
    let px := applyChunk chunk state.prevPixel state.index
    let hashIdx := hashColor px
    let newState := { state with
      prevPixel := px
      index := state.index.set! hashIdx.toNat px
    }
    (newState, [px])

-- Decode all pixels from byte stream
partial def decodePixelsAux (bytes : List UInt8) (state : QOIState) (pixels : List RGBA) (pos : Nat) (totalPixels : Nat) : Option (List RGBA) :=
  if pixels.length >= totalPixels then
    some pixels.reverse
  else if pos >= bytes.length then
    none
  else
    match decodeChunk bytes pos with
    | none => none
    | some (chunk, bytesConsumed) =>
      let (newState, newPixels) := processChunk state chunk
      decodePixelsAux bytes newState (newPixels.reverse ++ pixels) (pos + bytesConsumed) totalPixels

-- Main decode function
def decode (bytes : List UInt8) : Option (QOIHeader × List RGBA) := do
  let (header, dataBytes) ← decodeHeader bytes
  let totalPixels := header.width.toNat * header.height.toNat
  let pixels ← decodePixelsAux dataBytes initQOIState [] 0 totalPixels
  some (header, pixels)

-- Read a QOI file from disk
def readQOIFile (path : System.FilePath) : IO (Option (QOIHeader × List RGBA)) := do
  try
    let contents ← IO.FS.readBinFile path
    let bytes := contents.data.toList
    return decode bytes
  catch e =>
    IO.eprintln s!"Error reading file: {e}"
    return none

-- Write a QOI file to disk
def writeQOIFile (path : System.FilePath) (width : UInt32) (height : UInt32) (channels : UInt8) (pixels : List RGBA) : IO Bool := do
  match encode width height channels pixels with
  | none =>
    IO.eprintln "Error: Failed to encode image"
    return false
  | some encoded =>
    try
      let byteArray := ByteArray.mk (Array.mk encoded)
      IO.FS.writeBinFile path byteArray
      return true
    catch e =>
      IO.eprintln s!"Error writing file: {e}"
      return false

end QOI.Basic
