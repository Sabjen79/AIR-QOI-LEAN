# QOI Format Encoder/Decoder in Lean 4

This project implements a complete encoder and decoder for the QOI (Quite OK Image) format in Lean 4.

## About QOI

QOI (Quite OK Image) is a lossless image compression format that offers fast encoding and decoding while achieving decent compression ratios. It's simpler than PNG but still effective.

Specification: https://qoiformat.org/

## Implementation

The implementation includes:

### Core Types

- **RGBA**: Represents a pixel with red, green, blue, and alpha channels (UInt8 each)
- **QOIHeader**: Contains image metadata (width, height, channels, colorspace)
- **EncoderState**: Tracks encoding state (previous pixel, run length, color index, output)
- **DecoderState**: Tracks decoding state (previous pixel, color index, decoded pixels)

### Encoding Operations

The encoder supports all QOI operations:

1. **QOI_OP_RGB** (0xfe): Encodes RGB values when alpha hasn't changed
2. **QOI_OP_RGBA** (0xff): Encodes full RGBA values when alpha changes
3. **QOI_OP_INDEX** (00xxxxxx): References a previously seen color from the 64-color index
4. **QOI_OP_DIFF** (01xxxxxx): Encodes small differences (-2 to +1 for each channel)
5. **QOI_OP_LUMA** (10xxxxxx): Encodes larger differences using green-based deltas
6. **QOI_OP_RUN** (11xxxxxx): Encodes runs of identical pixels (up to 62 pixels)

### Main Functions

- `encode`: Encodes a list of RGBA pixels into QOI format bytes
- `decode`: Decodes QOI format bytes back into a header and pixel list
- `createTestImage`: Helper function to generate test images

## Usage

### Building

```bash
lake build
```

### Running the Test

```bash
lake exe qoi
```

The test program creates a 4x4 test image with various colors, encodes it to QOI format, decodes it back, and verifies that the round-trip is successful.

### Example Output

```
QOI Encoder/Decoder Test
========================

Original image: 4x4, 4 channels
Number of pixels: 16
Encoded size: 44 bytes
Compression ratio: 68.750000%

Decoding successful!
Decoded image: 4x4, 4 channels
Number of decoded pixels: 16
✓ Round-trip successful: decoded pixels match original!
```

### Using in Your Code

```lean
import QOI
open QOI.Basic

-- Create some pixels
let pixels : List RGBA := [
  { r := 255, g := 0, b := 0, a := 255 },  -- Red
  { r := 0, g := 255, b := 0, a := 255 },  -- Green
  { r := 0, g := 0, b := 255, a := 255 }   -- Blue
]

-- Encode to QOI format
match encode 3 1 4 pixels with
| some encoded => 
  -- Do something with encoded bytes
  -- Decode back
  match decode encoded with
  | some (header, decodedPixels) => 
    -- Use the decoded pixels
    ...
  | none => -- Handle decode error
| none => -- Handle encode error
```

## Implementation Details

### Color Hashing

QOI uses a simple hash function to index previously seen colors:
```
hash = (r * 3 + g * 5 + b * 7 + a * 11) mod 64
```

This creates a 64-entry cache of recently used colors, allowing efficient reference to repeated colors.

### Encoding Strategy

The encoder chooses the most efficient operation for each pixel:

1. Check if pixel equals previous → extend run
2. Check if pixel is in index → use QOI_OP_INDEX
3. If alpha unchanged:
   - Try QOI_OP_DIFF for small differences
   - Try QOI_OP_LUMA for medium differences
   - Fall back to QOI_OP_RGB
4. If alpha changed → use QOI_OP_RGBA

### Decoding

The decoder is implemented as a partial recursive function that processes the byte stream operation by operation, maintaining state and accumulating decoded pixels.

## Features

- ✅ Complete QOI format support
- ✅ All encoding operations (RGB, RGBA, INDEX, DIFF, LUMA, RUN)
- ✅ Full decoding support
- ✅ Proper header encoding/decoding with magic bytes
- ✅ Round-trip verification
- ✅ Type-safe implementation using Lean 4's strong type system
- ✅ Functional programming style with immutable data structures

## Project Structure

```
QOI/
├── QOI.lean           # Main module file
├── QOI/
│   └── Basic.lean     # Core implementation
├── Main.lean          # Test/demo program
├── lakefile.toml      # Build configuration
└── README.md          # This file
```

## License

This is an educational implementation for learning purposes.