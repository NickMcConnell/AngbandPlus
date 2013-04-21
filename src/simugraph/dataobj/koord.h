/* koord.h
 *
 * Copyright (c) 2001 Hansjörg Malthaner
 * hansjoerg.malthaner@gmx.de
 *
 * This file is part of the Simugraph graphics engine.
 *
 *
 * This file may be copied and modified freely so long as the above credits,
 * this paragraph, and the below disclaimer of warranty are retained; no
 * financial profit is derived from said modification or copying; and all
 * licensing rights to any modifications are granted to the original author,
 * Hansjörg Malthaner.
 *
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 * GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#ifndef koord_h
#define koord_h
               
#include <stdio.h>

 
/**
 * 2d koordinaten
 *
 * @author Hj. Malthaner
 */
class koord
{
private:
    static const short xtab[16];
    static const short ytab[16];

public:
    short x, y;

    koord() {x=y=0;};
    koord(int xp, int yp) {x=xp; y=yp;};
    koord(short xp, short yp) {x=xp; y=yp;};
    koord(FILE *file);
    koord(int ribi);

    void speichern(FILE *file);
    void laden(FILE *file);

    inline bool operator== (const koord & k) const {return (this->x == k.x && this->y == k.y);};
};

inline koord operator+ (const koord & a, const koord & b)
{
    return koord(a.x+b.x, a.y+b.y);
} 

inline koord operator- (const koord & a, const koord & b)
{
    return koord(a.x-b.x, a.y-b.y);
} 

inline const koord& operator+= (koord & a, const koord & b)
{
    a.x+=b.x;
    a.y+=b.y;
    return a;
} 

inline const koord& operator-= (koord & a, const koord & b)
{
    a.x-=b.x;
    a.y-=b.y;
    return a;
} 

inline bool operator!= (const koord & a, const koord & b)
{
    return (a.x != b.x) || (a.y != b.y);
} 

inline koord operator- (koord & a)
{
    return koord(-a.x, -a.y);
} 

/**
 * 2x2 matrices
 *
 * @author Hj. Malthaner
 */
class matrix
{    
public:
    short a,b,c,d;

    matrix() {a=b=c=d=0;};
    matrix(int aa, int bb, int cc, int dd) {
	a = aa;
	b = bb;
	c = cc;
	d = dd;
    }
};

inline koord operator* (const matrix & m, const koord & a)
{
    return koord(m.a*a.x+m.b*a.y, m.c*a.x+m.d*a.y);
} 

#endif  
