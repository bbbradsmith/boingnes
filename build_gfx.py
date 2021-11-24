#!/usr/bin/env python3

import PIL.Image
import os.path
from functools import total_ordering

OUTPUT_DIR = "temp"

#
# Indexed images
#

# set image palette from RGB tuples
def index_setpal(img,palette): 
    linpal = [p for tup in palette for p in tup[0:3]]
    img.putpalette(linpal)

# load and convert image to indexed palette
def index_img_load(filename, palette):
    src = PIL.Image.open(filename)
    dst = PIL.Image.new("P",src.size,color=0)
    for y in range(src.size[1]):
        for x in range(src.size[0]):
            p = src.getpixel((x,y))
            mag = ((255**2)*3)+1
            mat = 0
            for i in range(len(palette)):
                m = sum([(a-b)**2 for (a,b) in zip(p,palette[i])])
                if m < mag: # better match
                    mat = i
                    mag = m
                    if m == 0: # perfect match
                        break
            dst.putpixel((x,y),mat)
    index_setpal(dst,palette)
    return dst

# creates an image that places an error color anywhere the two images are different
# returns (count of error pixels, error image)
def index_img_cmp(imga,imgb,palette,error_color=(255,0,255)):
    assert imga.width == imgb.width and imga.height == imgb.height
    img = PIL.Image.new("P",(imga.width,imga.height),len(palette))
    count = 0
    for y in range(0,imga.height):
        for x in range(0,imga.width):
            pa = imga.getpixel((x,y))
            pb = imgb.getpixel((x,y))
            if pa == pb:
                img.putpixel((x,y),pa)
                count += 1
    index_setpal(img,list(palette)+[error_color])
    return ((imga.width * imga.height) - count, img)

#
# CHR tiles
#

# tile class holds 16 bytes of CHR data
@total_ordering
class Tile:
    def __init__(self,d=([0]*16)):
        self.d = bytes(d)

    # create tile from indexed image (only 2 bits of index are observed)
    def grab(img,x,y):
        c = [0]*16
        for j in range(0,8):
            for i in range(0,8):
                p = img.getpixel((x+i,y+j)) & 3
                c[0+j] = (c[0+j] << 1) | (p &  1)
                c[8+j] = (c[8+j] << 1) | (p >> 1)
        return Tile(c)

    # paste tile onto image
    def draw(self,img,x,y,a):
        for j in range(0,8):
            for i in range(0,8):
                p = ( (self.d[j+0] >> (7-i))       & 1) | \
                    (((self.d[j+8] >> (7-i)) << 1) & 2)
                img.putpixel((x+i,y+j),p+(a<<2))

    # paste tile onto image with transparent 0
    def draw_sprite(self,img,x,y,a):
        for j in range(0,8):
            for i in range(0,8):
                p = ( (self.d[j+0] >> (7-i))       & 1) | \
                    (((self.d[j+8] >> (7-i)) << 1) & 2)
                if p > 0:
                    img.putpixel((x+i,y+j),p+(a<<2))

    # flips
    def flipx(self):
        c = []
        for j in range(0,16):
            a = self.d[j]
            b = 0
            for i in range(8):
                b = (b << 1) | (a & 1)
                a >>= 1
            c.append(b)
        return Tile(c)
    def flipy(self):
        return Tile(self.d[0:8].reverse() + self.d[8:16].reverse())
    def flipxy(self):
        return flipx(flipy(self))

    # swap colours 2 and 3
    def swap23(self):
        c = bytearray(self.d)
        for j in range(0,8):
            c[0+j] ^= c[8+j] # if high bit is set, flip low bit 0123 => 0132
        return Tile(c)

    # comparisons
    def __eq__(self,other):
        return self.d == other.d
    def __ne__(self,other):
        return not (self.d == other.d)
    def __lt__(self,other):
        return self.d < other.d

# convert list of Tile to binary blob
def tile_dump(tiles):
    d = bytearray()
    for t in tiles:
        d.extend(t.d)
    return d

# convert CHR Tile list into RGB image using given 4-colour palette
def tiles_to_img(tiles, palette, width=16, horizontal=True):
    count = len(tiles)
    columns = width
    rows = (count + (width-1)) // width
    if not horizontal:
        rows, columns = columns, rows
    img = PIL.Image.new("P",(width*8,rows*8),0)
    for t in range(0,count):
        xo = (t % width) * 8
        yo = (t // width) * 8
        if not horizontal:
            yo, xo = xo, yo
        to = t * 16
        tiles[t].draw(img,xo,yo,0)
    index_setpal(img,palette)
    return img

#
# Nametable generation
#

# deduce palette attribute for 8x8 pixel tile
def grab_att(img,tx,ty): # if any non-0 colour in tile, use it as attribute index
    for y in range(0,8):
        for x in range(0,8):
            p = img.getpixel((tx+x,ty+y))
            if p != 0:
                return p >> 2
    return 0

# makes nametable data from indexed 16-colour image
# returns (nametable tile indices, packed attributes, Tile array)
# Note: will modify tiles (append) if it is passed as an argument.
def make_nametable(img,tiles=[]):
    nmt = []
    att = []
    tw = img.width//8
    th = img.height//8
    aw = ((tw+3)//4)*4 # pad attributes to 4 tile regions
    ah = ((th+3)//4)*4
    for ty in range(0,th):
        for tx in range(0,tw):
            ox = tx*8
            oy = ty*8
            t = Tile.grab(img,ox,oy)
            a = grab_att(img,ox,oy)
            if t in tiles:
                nmt.append(tiles.index(t))
            else:
                nmt.append(len(tiles))
                tiles.append(t)
            att.append(a)
        for i in range(tw,aw): # pad to 4
            att.append(0)
    for j in range(th,ah): # pad to 4
        att.extend([0]*aw)
    attbin = []
    for j in range(0,ah,4):
        for i in range(0,aw,4):
            b = 0
            for k in range(0,4):
                ox = i+(k &1) * 2 # select 2x2 quadrant for 4x4 byte area
                oy = j+(k>>1) * 2
                a = max( # pick any nonzero attribute in the 2x2 tile area area
                    att[(oy+0)*aw+ox+0],
                    att[(oy+0)*aw+ox+1],
                    att[(oy+1)*aw+ox+0],
                    att[(oy+1)*aw+ox+1])
                b = (b >> 2) | (a << 6)
            attbin.append(b)
    return (bytearray(nmt),bytearray(attbin),tiles)

# creates an indexed palette image from nametable data
def nametable_to_img(nmt,att,tiles,palette,tw=32):
    th = len(nmt)//tw
    abw = ((tw+3)//4)
    img = PIL.Image.new("P",(tw*8,th*8))
    for ty in range(0,th):
        for tx in range(0,tw):
            ab = att[((ty//4)*abw) + (tx//4)]
            a = (ab >> ((tx & 2) + ((ty & 2) * 2))) & 3
            tiles[nmt[(ty*tw)+tx]].draw(img,tx*8,ty*8,a)
    index_setpal(img,palette)
    return img

def convert_nametable_img(filename,palette,tiles=[]):
    print("convert nametable: " + filename)
    filebase = os.path.splitext(os.path.basename(filename))[0]
    src = index_img_load(filename,palette)
    (n,a,tiles) = make_nametable(src,tiles)
    filenmt = os.path.join(OUTPUT_DIR,filebase+".nmt") # nametable data
    filepng = os.path.join(OUTPUT_DIR,filebase+".nmt.png") # nametable visualized
    filecmp = os.path.join(OUTPUT_DIR,filebase+".cmp.png") # error comparison
    print("nametable data: " + filenmt)
    open(filenmt,"wb").write(n+a)
    print("nametable preview: " + filepng)
    dst = nametable_to_img(n,a,tiles,palette)
    dst.save(filepng)
    (count,cmpimg) = index_img_cmp(src,dst,palette)
    if count == 0:
        print("no errors.")
    else:
        print("%d pixel errors: %s" % (count,filecmp))
        cmpimg.save(filecmp)
    return tiles

#
# TEST!
#

grey_pal = [
    (  0,  0,  0),( 85, 85, 85),(170,170,170),(255,255,255),
    ]

bg_pal = [
    (  0,  0,  0),(170,170,170),(170,  0,170),(255,255,255),
    (  0,  0,  0),( 33, 64, 64),( 37,223,192),(255,255,255),
    ]

bg_tiles = convert_nametable_img("gfx/amiga.png",bg_pal)
bg_tiles = convert_nametable_img("gfx/atari.png",bg_pal,bg_tiles)

filebgchr = os.path.join(OUTPUT_DIR,"bg.chr")
filebgpng = os.path.join(OUTPUT_DIR,"bg.chr.png")
print("%d BG tiles: %s" % (len(bg_tiles),filebgchr))
open(filebgchr,"wb").write(tile_dump(bg_tiles))
print("BG tile preview:" + filebgpng)
tiles_to_img(bg_tiles,grey_pal).save(filebgpng)
