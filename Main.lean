import QOI

open QOI.Basic

-- Decode: QOI file to PPM file
def qoiToPpm (qoiFile ppmFile : String) : IO Unit := do
  let qoiPath := System.FilePath.mk qoiFile
  let ppmPath := System.FilePath.mk ppmFile

  let result ← readQOIFile qoiPath
  match result with
  | none => IO.Process.exit 1
  | some (header, pixels) =>
    if ← QOI.PPM.writePPM ppmPath header.width header.height pixels then
      pure ()
    else
      IO.Process.exit 1

-- Encode: PPM file to QOI file
def ppmToQoi (ppmFile qoiFile : String) : IO Unit := do
  let ppmPath := System.FilePath.mk ppmFile
  let qoiPath := System.FilePath.mk qoiFile

  let result ← QOI.PPM.readPPM ppmPath
  match result with
  | none => IO.Process.exit 1
  | some (width, height, channels, pixels) =>
    if ← writeQOIFile qoiPath width height channels pixels then
      pure ()
    else
      IO.Process.exit 1

def main (args : List String) : IO Unit := do
  match args with
  | ["decode", qoiFile, ppmFile] => qoiToPpm qoiFile ppmFile
  | ["encode", ppmFile, qoiFile] => ppmToQoi ppmFile qoiFile
  | _ =>
    IO.eprintln "Usage:"
    IO.eprintln "  qoi decode <input.qoi> <output.ppm>"
    IO.eprintln "  qoi encode <input.ppm> <output.qoi>"
    IO.Process.exit 1
