#lang magic

# the magic in this file is designed to return a string corresponding to a valid 'kind' argument for bitmap%
# examples: "png" or "gif/alpha"

# PNG
0            name            png-transparent
>4           belong          !0x49444154
>>4          belong          !0x74524E53
>>>(0.L+12)  use             png-transparent
>>4          belong          0x74524E53          \b/alpha

0	name		png-alpha
>9	byte		4		\b/alpha
>9	byte		6		\b/alpha
>9	byte		0		
>>17    use             png-transparent
>9	byte		2		
>>17    use             png-transparent
>9	byte		3		
>>17    use             png-transparent

0	string		\x89PNG\x0d\x0a\x1a\x0a\x00\x00\x00\x0DIHDR	png
>16	use		png-alpha

# GIF
0       name            gif-gce
>0      byte            0x21            
>>1     byte            0xf9            
>>>3    byte&0x01       =0x01           \b/alpha

0	string		GIF8		gif
>4	string		7a		\b
>4	string		9a		
>>10	byte&0x07	=0x07		
>>>781  use             gif-gce
>>10	byte&0x07	=0x00		
>>>19   use             gif-gce
>>10	byte&0x07	=0x01		
>>>25   use             gif-gce
>>10	byte&0x07	=0x02		
>>>37   use             gif-gce
>>10	byte&0x07	=0x03		
>>>61   use             gif-gce
>>10	byte&0x07	=0x04		
>>>109   use             gif-gce
>>10	byte&0x07	=0x05		
>>>205   use             gif-gce
>>10	byte&0x07	=0x06		
>>>397   use             gif-gce        

# JPEG
0	beshort		0xffd8		jpeg

# BMP
0	string		BM              bitmap

