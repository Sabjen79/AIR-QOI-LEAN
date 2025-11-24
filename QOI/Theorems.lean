-- Theorems about QOI encoding/decoding properties

import QOI.Basic

namespace QOI.Theorems

open QOI.Basic

/--
  Round-trip theorem: Encoding and then decoding returns the original input.

  For any valid image data (width, height, channels, pixels), if we:
  1. Encode the image to QOI format
  2. Decode the resulting bytes back

  Then we should get back the same header and pixel data.
-/
theorem encode_decode_roundtrip
    (width : UInt32)
    (height : UInt32)
    (channels : UInt8)
    (pixels : List RGBA)
    (h_channels : channels = 3 ∨ channels = 4)
    (h_length : pixels.length = width.toNat * height.toNat) :
    ∀ encoded, encode width height channels pixels = some encoded →
    ∃ (header : QOIHeader) (decoded_pixels : List RGBA),
      decode encoded = some (header, decoded_pixels) ∧
      header.width = width ∧
      header.height = height ∧
      header.channels = channels ∧
      header.colorspace = 0 ∧
      decoded_pixels = pixels := by
  sorry

theorem encode_does_not_fail
    (width : UInt32)
    (height : UInt32)
    (channels : UInt8)
    (pixels : List RGBA)
    (h_channels : channels = 3 ∨ channels = 4)
    (h_length : pixels.length = width.toNat * height.toNat) :
    ∃ encoded, encode width height channels pixels = some encoded :=
  by
    exists (
      let header : QOIHeader := { width, height, channels, colorspace := 0 }
      (encodeHeader header ++ encodePixels pixels)
    )
    simp [encode]
    simp [guard]
    grind
/--
  Chunk encoding and decoding are inverse operations for valid chunks.

  For any QOI chunk, encoding it to bytes and then decoding those bytes
  should return the original chunk.
-/
theorem encodeChunk_decodeChunk_inverse
    (chunk : QOIChunk) :
    ∃ bytesConsumed,
      decodeChunk (encodeChunk chunk) 0 = some (chunk, bytesConsumed) := by
  sorry

/--
  Applying a chunk after encoding/decoding preserves the pixel transformation.

  If we encode a chunk, decode it, and apply it to get a new pixel,
  the result should be the same as applying the original chunk.
-/
theorem applyChunk_preserves_transformation
    (chunk : QOIChunk)
    (prevPixel : RGBA)
    (index : Array RGBA)
    (h_index_size : index.size = 64) :
    ∃ decoded_chunk bytesConsumed,
      decodeChunk (encodeChunk chunk) 0 = some (decoded_chunk, bytesConsumed) →
      applyChunk decoded_chunk prevPixel index = applyChunk chunk prevPixel index := by
  sorry

/--
  Header encoding and decoding are inverse operations.

  For any valid QOI header, encoding it to bytes and then decoding those bytes
  should return the original header.
-/
theorem encodeHeader_decodeHeader_inverse
    (header : QOIHeader)
    (h_channels : header.channels = 3 ∨ header.channels = 4)
    (h_colorspace : header.colorspace ≤ 1) :
    ∃ remaining,
      decodeHeader (encodeHeader header) = some (header, remaining) ∧
      remaining = [] := by
  sorry

end QOI.Theorems
