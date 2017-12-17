<?php
    //-----------------------------------------------------------------------------
    // Nubo Server Application
    // Copyright (c) 2017, Pascal Levy
    //
    // Permission is hereby granted, free of charge, to any person obtaining a copy
    // of this software and associated documentation files (the "Software"), to deal
    // in the Software without restriction, including without limitation the rights
    // to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    // copies of the Software, and to permit persons to whom the Software is
    // furnished to do so, subject to the following conditions:
    //
    // The above copyright notice and this permission notice shall be included in
    // all copies or substantial portions of the Software.
    //
    // THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    // IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    // FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    // AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    // LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    // OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    // THE SOFTWARE.
    //-----------------------------------------------------------------------------

    require_once('exception.php');

    //-----------------------------------------------------------------------------
    // Class to MsgPack encode a value.
    //-----------------------------------------------------------------------------

    class MsgPacker {

        const UTF8_REGEX = '/\A(?:
              [\x00-\x7F]++                      # ASCII
            | [\xC2-\xDF][\x80-\xBF]             # non-overlong 2-byte
            |  \xE0[\xA0-\xBF][\x80-\xBF]        # excluding overlongs
            | [\xE1-\xEC\xEE\xEF][\x80-\xBF]{2}  # straight 3-byte
            |  \xED[\x80-\x9F][\x80-\xBF]        # excluding surrogates
            |  \xF0[\x90-\xBF][\x80-\xBF]{2}     # planes 1-3
            | [\xF1-\xF3][\x80-\xBF]{3}          # planes 4-15
            |  \xF4[\x80-\x8F][\x80-\xBF]{2}     # plane 16
            )*+\z/x';

        public function pack($value) {
            if (is_int($value)) {
                return $this->packInt($value);
            } else if (is_string($value)) {
                return preg_match(self::UTF8_REGEX, $value) ? $this->packStr($value) : $this->packBin($value);
            } else if (is_array($value)) {
                return array_values($value) === $value ? $this->packArray($value) : $this->packMap($value);
            } else if ($value === null) {
                return "\xC0";
            } else if (is_bool($value)) {
                return $value ? "\xC3" : "\xC2";
            } else if (is_float($value)) {
                return "\xCB" . strrev(pack('d', $value));
            } else {
                throw new LogicException('Unsupported data type');
            }
        }

        private function packArray(array $array) {
            $size = count($array);
            if ($size < 16) {
                $data = chr(0x90 | $size);
            } else if ($size < 65536) {
                $data = pack('Cn', 0xDC, $size);
            } else {
                $data = pack('CN', 0xDD, $size);
            }
            foreach ($array as $val) {
                $data .= $this->pack($val);
            }
            return $data;
        }

        private function packMap(array $map) {
            $size = count($map);
            if ($size < 16) {
                $data = chr(0x80 | $size);
            } else if ($size < 65536) {
                $data = pack('Cn', 0xDE, $size);
            } else {
                $data = pack('CN', 0xDF, $size);
            }
            foreach ($map as $key => $val) {
                $data .= $this->pack($key);
                $data .= $this->pack($val);
            }
            return $data;
        }

        private function packStr($str) {
            $len = strlen($str);
            if ($len < 32) {
                $data = chr(0xA0 | $len);
            } else if ($len < 256) {
                $data = "\xD9" . chr($len);
            } else if ($len < 65536) {
                $data = pack('Cn', 0xDA, $len);
            } else {
                $data = pack('CN', 0xDB, $len);
            }
            return $data . $str;
        }

        private function packBin($str) {
            $len = strlen($str);
            if ($len < 256) {
                $data = "\xC4" . chr($len);
            } else if ($len < 65536) {
                $data = pack('Cn', 0xC5, $len);
            } else {
                $data = pack('CN', 0xC6, $len);
            }
            return $data . $str;
        }

        private function packInt($num) {
            if ($num < -2147483648) {
                return pack('CJ', 0xD3, $num);
            } else if ($num < -32768) {
                return pack('CN', 0xD2, $num);
            } else if ($num < -128) {
                return pack('Cn', 0xD1, $num);
            } else if ($num < -32) {
                return "\xD0" . chr($num);
            } else if ($num < 128) {
                return chr($num);
            } else if ($num < 256) {
                return "\xCC" . chr($num);
            } else if ($num < 65536) {
                return pack('Cn', 0xCD, $num);
            } else if ($num < 4294967296) {
                return pack('CN', 0xCE, $num);
            } else {
                return pack('CJ', 0xCF, $num);
            }
        }
    }

    //-----------------------------------------------------------------------------
    // Class to MsgPack decode a value.
    //-----------------------------------------------------------------------------

    class MsgUnpacker {

        private $buffer;
        private $offset;

        public function __construct($buffer) {
            $this->buffer = $buffer;
            $this->offset = 0;
        }

        public function unpack() {
            if (!isset($this->buffer[$this->offset])) {
                throw new NuboException(ERROR_MSGPACK);
            }
            $c = ord($this->buffer[$this->offset++]);
            if ($c <= 0x7F) {
                return $c;
            } else if ($c >= 0xA0 && $c <= 0xBF) {
                return $this->unpackString($c & 0x1F);
            } else if ($c >= 0x90 && $c <= 0x9F) {
                return $this->unpackArray($c & 0x0F);
            } else if ($c >= 0x80 && $c <= 0x8F) {
                return $this->unpackMap($c & 0x0F);
            } else if ($c >= 0xE0) {
                return $c - 256;
            } else {
                switch ($c) {
                    case 0xc0: return null;
                    case 0xc2: return false;
                    case 0xc3: return true;
                    case 0xc4: return $this->unpackString($this->unpackInt8(false));
                    case 0xc5: return $this->unpackString($this->unpackInt16(false));
                    case 0xc6: return $this->unpackString($this->unpackInt32(false));
                    case 0xcb: return $this->unpackFloat64();
                    case 0xcc: return $this->unpackInt8(false);
                    case 0xcd: return $this->unpackInt16(false);
                    case 0xce: return $this->unpackInt32(false);
                    case 0xcf: return $this->unpackInt64(false);
                    case 0xd0: return $this->unpackInt8(true);
                    case 0xd1: return $this->unpackInt16(true);
                    case 0xd2: return $this->unpackInt32(true);
                    case 0xd3: return $this->unpackInt64(true);
                    case 0xd9: return $this->unpackString($this->unpackInt8(false));
                    case 0xda: return $this->unpackString($this->unpackInt16(false));
                    case 0xdb: return $this->unpackString($this->unpackInt32(false));
                    case 0xdc: return $this->unpackArray($this->unpackInt16(false));
                    case 0xdd: return $this->unpackArray($this->unpackInt32(false));
                    case 0xde: return $this->unpackMap($this->unpackInt16(false));
                    case 0xdf: return $this->unpackMap($this->unpackInt32(false));
                    default:   throw new NuboException(ERROR_MSGPACK);
                }
            }
        }

        private function unpackInt8($signed) {
            if (!isset($this->buffer[$this->offset])) {
                throw new NuboException(ERROR_MSGPACK);
            }
            $num = ord($this->buffer[$this->offset]);
            $this->offset += 1;
            return ($signed && $num >= 128) ? $num - 256 : $num;
        }

        private function unpackInt16($signed) {
            if (!isset($this->buffer[$this->offset + 1])) {
                throw new NuboException(ERROR_MSGPACK);
            }
            $num = ord($this->buffer[$this->offset]) << 8 | ord($this->buffer[$this->offset + 1]);
            $this->offset += 2;
            return ($signed && $num >= 32768) ? $num - 65536 : $num;
        }

        private function unpackInt32($signed) {
            if (!isset($this->buffer[$this->offset + 3])) {
                throw new NuboException(ERROR_MSGPACK);
            }
            $num = unpack('N', substr($this->buffer, $this->offset, 4))[1];
            $this->offset += 4;
            return ($signed && $num >= 2147483648) ? $num - 4294967296 : $num;
        }

        private function unpackInt64($signed) {
            if (!isset($this->buffer[$this->offset + 7])) {
                throw new NuboException(ERROR_MSGPACK);
            }
            $num = unpack('J', substr($this->buffer, $this->offset, 8))[1];
            $this->offset += 8;
            return $num;
        }

        private function unpackFloat64() {
            if (!isset($this->buffer[$this->offset + 7])) {
                throw new NuboException(ERROR_MSGPACK);
            }
            $num = unpack('d', strrev(substr($this->buffer, $this->offset, 8)));
            $this->offset += 8;
            return $num[1];
        }

        private function unpackString($length) {
            if ($length) {
                if (!isset($this->buffer[$this->offset + $length - 1])) {
                    throw new NuboException(ERROR_MSGPACK);
                }
                $str = substr($this->buffer, $this->offset, $length);
                $this->offset += $length;
            } else {
                $str = '';
            }
            return $str;
        }

        private function unpackArray($size) {
            $array = [];
            while ($size--) {
                $array[] = $this->unpack();
            }
            return $array;
        }

        private function unpackMap($size) {
            $object = [];
            while ($size--) {
                $k = $this->unpack();
                $v = $this->unpack();
                $object[$k] = $v;
            }
            return $object;
        }
    }

    //-----------------------------------------------------------------------------
?>
