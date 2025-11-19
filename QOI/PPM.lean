-- PPM (Portable Pixmap) Format Support
-- PPM is a simple uncompressed image format

import QOI.Basic

namespace QOI.PPM

open QOI.Basic

-- Helper: Extract ASCII string until newline from bytes
def extractLine (bytes : List UInt8) (start : Nat) : Option (String × Nat) := do
  let rec findNewline (pos : Nat) : Option Nat :=
    if pos >= bytes.length then none
    else
      match bytes[pos]? with
      | some b => if b == 10 then some pos else findNewline (pos + 1)  -- 10 is '\n'
      | none => none

  let endPos ← findNewline start
  let lineBytes := bytes.drop start |>.take (endPos - start)

  -- Convert to ASCII string (safe since PPM headers are ASCII)
  let chars := lineBytes.map (fun b => Char.ofNat b.toNat)
  some (String.mk chars, endPos + 1)

-- Parse PPM header and return (width, height, header_size)
def parsePPMHeader (bytes : List UInt8) : Option (UInt32 × UInt32 × Nat) := do
  -- Read magic number
  let (magic, pos1) ← extractLine bytes 0
  guard (magic.trim == "P6")

  -- Skip comment lines and read dimensions
  let mut pos := pos1
  let mut dimLine := ""

  -- Keep reading until we get a non-comment, non-empty line
  while true do
    let (line, nextPos) ← extractLine bytes pos
    let trimmed := line.trim
    if trimmed.startsWith "#" || trimmed == "" then
      pos := nextPos
    else
      dimLine := trimmed
      pos := nextPos
      break

  -- Parse width and height
  let dims := dimLine.splitOn " " |> List.filter (fun s => s != "")
  guard (dims.length == 2)
  let width ← (dims[0]!).toNat?
  let height ← (dims[1]!).toNat?

  -- Read max value (skip comments)
  let mut maxVal := ""
  while true do
    let (line, nextPos) ← extractLine bytes pos
    let trimmed := line.trim
    if trimmed.startsWith "#" || trimmed == "" then
      pos := nextPos
    else
      maxVal := trimmed
      pos := nextPos
      break

  guard (maxVal == "255")

  -- pos now points to the start of binary pixel data
  some (width.toUInt32, height.toUInt32, pos)

-- Read PPM file and return (width, height, channels, pixels)
def readPPM (path : System.FilePath) : IO (Option (UInt32 × UInt32 × UInt8 × List RGBA)) := do
  try
    let contents ← IO.FS.readBinFile path
    let bytes := contents.data.toList

    let parseResult := parsePPMHeader bytes
    match parseResult with
    | none =>
      IO.eprintln "Error: Invalid PPM header"
      return none
    | some (width, height, headerSize) =>
      let pixelBytes := bytes.drop headerSize
      let expectedSize := width.toNat * height.toNat * 3

      if pixelBytes.length < expectedSize then
        IO.eprintln "Error: Insufficient pixel data"
        return none

      -- Convert RGB bytes to RGBA pixels
      let mut pixels : List RGBA := []
      let mut i := 0

      while i + 2 < pixelBytes.length && pixels.length < width.toNat * height.toNat do
        match pixelBytes[i]?, pixelBytes[i + 1]?, pixelBytes[i + 2]? with
        | some r, some g, some b =>
          pixels := pixels ++ [{ r, g, b, a := 255 }]
          i := i + 3
        | _, _, _ => break

      if pixels.length != width.toNat * height.toNat then
        IO.eprintln "Error: Pixel count mismatch"
        return none

      let channels : UInt8 := 4
      return some (width, height, channels, pixels)
  catch e =>
    IO.eprintln s!"Error reading PPM file: {e}"
    return none

-- Write image as PPM (Portable Pixmap) format
def writePPM (path : System.FilePath) (width : UInt32) (height : UInt32) (pixels : List RGBA) : IO Bool := do
  try
    -- PPM header (P6 = binary format)
    let header := s!"P6\n{width} {height}\n255\n"

    -- Convert RGBA pixels to RGB bytes (PPM doesn't support alpha)
    let mut pixelBytes : List UInt8 := []
    for px in pixels do
      pixelBytes := pixelBytes ++ [px.r, px.g, px.b]

    let byteArray := ByteArray.mk (Array.mk (header.toUTF8.data.toList ++ pixelBytes))
    IO.FS.writeBinFile path byteArray
    return true
  catch e =>
    IO.eprintln s!"Error writing PPM file: {e}"
    return false

end QOI.PPM
