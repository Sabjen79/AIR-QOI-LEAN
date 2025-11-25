# QOI Format Encoder/Decoder in Lean 4

This project implements a complete encoder and decoder for the QOI (Quite OK Image) format in Lean 4.

# Usage

To build the executable:
```
lake build
```

To use it:
```js
lake exe qoi decode <input.qoi> <output.ppm>
lake exe qoi encode <input.ppm> <output.qoi>
```