/* karte_modell.h
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

#ifndef ifc_karte_modell_h
#define ifc_karte_modell_h

#include "../dataobj/koord.h"


/**
 * Schnittstelle zwischen Weltansicht und Weltmodell.
 *
 * @author Hj. Malthaner
 * @version $Revsision$
 */
class karte_modell_t
{
private:
    /**
     * manche actionen machen eine neuanzeige der ganzen karte notwendig
     * diese aktionen müssen dirty auf true setzen
     */
    bool dirty;


    koord ij_off; 

public:
    /**
     * Setzt das globale dirty flag.
     * @author Hj. Malthaner
     */
    void setze_dirty() {dirty=true;};


    /**
     * Löscht das globale dirty flag.
     * @author Hj. Malthaner
     */
    void setze_dirty_zurueck() {dirty=false;};


    /**
     * Frägt das globale dirty flag ab.
     * @author Hj. Malthaner
     */
    bool ist_dirty() const {return dirty;};


    /**
     * Ermittelt ij-Koordinate des Blickpunkts auf der Karte.
     * @author Hj. Malthaner
     */
    virtual koord gib_ij_off() const {return ij_off;};


    /**
     * Setzt i-Koordinate des Blickpunkts auf der Karte.
     * @author Hj. Malthaner
     */
    void setze_ij_off(koord ij) {ij_off=ij; dirty=true;};


    /**
     * Ermittelt x-Offset gescrollter Karte
     * @author Hj. Malthaner
     */
    virtual int gib_x_off() const = 0;


    /**
     * Ermittelt y-Offset gescrollter Karte
     * @author Hj. Malthaner
     */
    virtual int gib_y_off() const = 0;

                                     
    /**
     * Holt den Grundwasserlevel der Karte
     *
     * @author Hj. Malthaner
     */
    virtual int gib_grundwasser() const = 0;



    /**
     * Holt Zustand des Planquadrats von Position pos.
     * @param pos Position des abgefragten Planquadrats
     * @return true, wenn das Planquadrat neu gezeichnet werden muss
     * @author Hj. Malthaner
     */
    virtual bool ist_dirty(koord pos) const = 0;


    /**
     * Setzt das dirty Flag eines Planquadrats zurück;
     * @author Hj. Malthaner
     */
    virtual void markiere_clean(koord pos) = 0;
};



#endif