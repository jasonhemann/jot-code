import Image

f = open('bv-out-file.txt')

img = Image.new( 'RGB', (1024,1024), "black")
pixels = img.load()
 
for i in range(img.size[0]):
    for j in range(img.size[1]):
        n = int(f.readline())
        b =  n & 255
        g = (n >> 8) & 255
        r = (n >> 16) & 255
        pixels[i,j] = (r, g, b)
 
img.show()
