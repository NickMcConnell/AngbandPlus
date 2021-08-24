/*
 * File: qt_statusbar.cpp
 *
 * Copyright (c) 2014  Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/qt_mainwindow.h>
#include <src/npp.h>
#include <src/player_command.h>
#include <QToolBar>


// Update and display the appropriate icons on the statusbar;
void MainWindow::update_statusbar(void)
{
    if (!character_generated) return;
    if (p_ptr->word_recall) recall->setVisible(TRUE);
    else recall->setVisible(FALSE);

    searching->setVisible(TRUE);
    if (p_ptr->searching)
    {
        searching->setIcon(QIcon(":icons/lib/icons/search.png"));
        searching->setToolTip("Click to stop searching");
    }
    else
    {
        searching->setIcon(QIcon(":icons/lib/icons/search_stop.png"));
        searching->setToolTip("Click to start searching.  This increases chance of finding hidden doors or traps, but slows the player down.");
    }

    if (p_ptr->timed[TMD_CUT])
    {
        int cut = p_ptr->cut_status();

        QString cut_text;

        status_cut->setVisible(TRUE);

        if (cut == CUT_MORTAL_WOUND)
        {
            cut_text = "Mortal";
            status_cut->setToolTip("This mortal wound is so severe it can only be cured by magic, and will cause 3 hit points damage per turn at normal speed.");
        }
        else if (cut == CUT_DEEP_GASH)
        {
            cut_text = "Deep Gash";
            status_cut->setToolTip("This deep gash causes 3 hit points damage per turn at normal speed.");
        }
        else if (cut == CUT_SEVERE)
        {
            cut_text = "Severe Cut";
            status_cut->setToolTip("This severe cut causes 2 hit points damage per turn at normal speed.");
        }
        else if (cut == CUT_NASTY)
        {
            cut_text = "Nasty Cut";
            status_cut->setToolTip("This nasty cut causes 1 hit point damage per turn at normal speed.");
        }
        else if (cut == 25)
        {
            cut_text = "Bad Cut";
            status_cut->setToolTip("This bad cut causes 1 hit point damage per turn at normal speed.");
        }
        else if (cut == 10)
        {
            cut_text = "Light Cut";
            status_cut->setToolTip("This light cut causes 1 hit point damage per turn at normal speed.");
        }
        else // Graze
        {
            cut_text = "Graze";
            status_cut->setToolTip("This graze causes 1 hit point damage per turn at normal speed.");
        }

        QPixmap pix = (QPixmap(":icons/lib/icons/cut.png"));
        QPainter painter(&pix);
        int height = pix.height();
        int width = pix.width();
        QPen pen = QPen(Qt::black, 4);
        QFont font = ui_main_window_font();
        font.setPointSize(height/5);
        font.setBold(TRUE);
        painter.setFont(font);
        painter.setPen(pen);
        painter.setOpacity(1);
        QRect rectangle = QRect(QPoint(0, height/2), QSize(width, height));
        painter.drawText(rectangle, Qt::AlignRight, cut_text);
        status_cut->setIcon(pix);
    }
    else status_cut->setVisible(FALSE);

    if (p_ptr->timed[TMD_STUN])
    {
        int stun = p_ptr->stun_status();

        QString stun_text;

        status_stun->setVisible(TRUE);

        if (stun == STUN_KNOCKED_OUT)
        {
            stun_text = "KO";
            status_stun->setToolTip("You are unconscious and will be unable to act until you wake up.");
        }
        else if (stun == STUN_HEAVY)
        {
            stun_text = "Heavy";
            status_stun->setToolTip("You are heavily stunned and are close to being knocked out.  Players who are knocked out rarely survive.  Combat and spellcasting failure rates increase while you are stunned.");
        }
        else // Light stun
        {
            stun_text = "Light";
            status_stun->setToolTip("You are lightly stunned.  Combat and spellcasting failure rates increase while you are stunned.");
        }

        QPixmap pix = (QPixmap(":icons/lib/icons/stun.png"));
        QPainter painter(&pix);
        int height = pix.height();
        int width = pix.width();
        QPen pen = QPen(Qt::black, 4);
        QFont font = ui_main_window_font();
        font.setPointSize(height/5);
        font.setBold(TRUE);
        painter.setFont(font);
        painter.setPen(pen);
        painter.setOpacity(1);
        QRect rectangle = QRect(QPoint(0, height/2), QSize(width, height));
        painter.drawText(rectangle, Qt::AlignRight, stun_text);
        status_stun->setIcon(pix);

    }
    else status_stun->setVisible(FALSE);

    if (TRUE)
    {
        int food = p_ptr->food;

        QString food_text;

        bool visibility = TRUE;

        if (food > PY_FOOD_MAX)
        {
            status_hunger->setIcon(QIcon(":icons/lib/icons/hunger_gorged.png"));
            food_text = "Gorged";
            status_hunger->setToolTip("You have eaten too much and will be temporarily slowed.");
        }
        else if (food > PY_FOOD_FULL)
        {
            status_hunger->setIcon(QIcon(":icons/lib/icons/hunger_full.png"));
            food_text = "Full";
            status_hunger->setToolTip("You are full.");
        }
        else if (food > PY_FOOD_ALERT)
        {
            visibility = FALSE;
        }
        else if (food > PY_FOOD_WEAK)
        {
            status_hunger->setIcon(QIcon(":icons/lib/icons/hunger_hungry.png"));
            food_text = "Hungry";
            status_hunger->setToolTip("You are hungry.");
        }
        else if (food > PY_FOOD_FAINT)
        {
            status_hunger->setIcon(QIcon(":icons/lib/icons/hunger_weak.png"));
            food_text = "Weak";
            status_hunger->setToolTip("You are weak.  Your regeneration rate will be slowed until you eat.");
        }
        else if (food > PY_FOOD_STARVE)
        {
            status_hunger->setIcon(QIcon(":icons/lib/icons/hunger_faint.png"));
            food_text = "Faint";
            status_hunger->setToolTip("You are faint from hunger.  Your regeneration rate will be greatly slowed until you eat.");
        }
        else // PY_FOOD_STARVE
        {
            status_hunger->setIcon(QIcon(":icons/lib/icons/hunger_starved.png"));
            food_text = "Starved";
            status_hunger->setToolTip("You are starving.  You cannot regenerate, and will take damage and have a chance pass out every turn until you eat something.");
        }

        if (visibility)
        {
            QPixmap pix = (QPixmap(":icons/lib/icons/hunger.png"));
            QPainter painter(&pix);
            int height = pix.height();
            int width = pix.width();
            QPen pen = QPen(Qt::black, 4);
            QFont font = ui_main_window_font();
            font.setPointSize(height/4);
            font.setBold(TRUE);
            painter.setFont(font);
            painter.setPen(pen);
            painter.setOpacity(1);
            QRect rectangle = QRect(QPoint(0, height/2), QSize(width, height));
            painter.drawText(rectangle, Qt::AlignRight, food_text);
            status_hunger->setIcon(pix);
        }

        status_hunger->setVisible(visibility);
    }

    if (p_ptr->new_spells)
    {
        QPixmap pix = (QPixmap(":icons/lib/icons/study.png"));
        QString text = (QString(" x%1").arg(p_ptr->new_spells));
        QPainter painter(&pix);
        int height = pix.height();
        int width = pix.width();
        QPen pen = QPen(Qt::white, 4);
        QFont font = ui_main_window_font();
        font.setPointSize(height/3);
        font.setBold(TRUE);
        painter.setFont(font);
        painter.setPen(pen);
        painter.setOpacity(1);
        QRect rectangle = QRect(QPoint(0, height/2), QSize(width, height));
        painter.drawText(rectangle, Qt::AlignRight, text);
        study->setIcon(pix);

        QString p = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);
        study->setToolTip(QString("Click to Study.  You can learn %1 new %2s.") .arg(p_ptr->new_spells) .arg(p));
        study->setVisible(TRUE);
    }
    else study->setVisible(FALSE);

    if (p_ptr->timed[TMD_BLIND]) blind->setVisible(TRUE);
    else blind->setVisible(FALSE);

    if (p_ptr->timed[TMD_PARALYZED]) paralyzed->setVisible(TRUE);
    else paralyzed->setVisible(FALSE);

    if (p_ptr->timed[TMD_CONFUSED]) confused->setVisible(TRUE);
    else confused->setVisible(FALSE);

    if (p_ptr->timed[TMD_AFRAID]) afraid->setVisible(TRUE);
    else afraid->setVisible(FALSE);

    if (p_ptr->timed[TMD_IMAGE]) hallucination->setVisible(TRUE);
    else hallucination->setVisible(FALSE);

    if (p_ptr->timed[TMD_POISONED]) poisoned->setVisible(TRUE);
    else poisoned->setVisible(FALSE);

    if (p_ptr->timed[TMD_PROTEVIL]) protect_evil->setVisible(TRUE);
    else protect_evil->setVisible(FALSE);

    if (p_ptr->timed[TMD_INVULN]) invulnerability->setVisible(TRUE);
    else invulnerability->setVisible(FALSE);

    if (p_ptr->timed[TMD_HERO]) hero->setVisible(TRUE);
    else hero->setVisible(FALSE);

    if (p_ptr->timed[TMD_BERSERK]) berzerk->setVisible(TRUE);
    else berzerk->setVisible(FALSE);

    if (p_ptr->timed[TMD_SHIELD]) shield->setVisible(TRUE);
    else shield->setVisible(FALSE);

    if (p_ptr->timed[TMD_BLESSED]) blessed->setVisible(TRUE);
    else blessed->setVisible(FALSE);

    if ((p_ptr->timed[TMD_SINVIS]) && !redundant_timed_event(TMD_SINVIS)) see_invisible->setVisible(TRUE);
    else see_invisible->setVisible(FALSE);

    if (p_ptr->timed[TMD_SINFRA]) infravision->setVisible(TRUE);
    else infravision->setVisible(FALSE);

    if (p_ptr->timed[TMD_OPP_FIRE] && !redundant_timed_event(TMD_OPP_FIRE)) resist_fire->setVisible(TRUE);
    else resist_fire->setVisible(FALSE);

    if (p_ptr->timed[TMD_OPP_COLD] && !redundant_timed_event(TMD_OPP_COLD)) resist_cold->setVisible(TRUE);
    else resist_cold->setVisible(FALSE);

    if (p_ptr->timed[TMD_OPP_ACID] && !redundant_timed_event(TMD_OPP_ACID)) resist_acid->setVisible(TRUE);
    else resist_acid->setVisible(FALSE);

    if (p_ptr->timed[TMD_OPP_ELEC] && !redundant_timed_event(TMD_OPP_ELEC)) resist_lightning->setVisible(TRUE);
    else resist_lightning->setVisible(FALSE);

    if (p_ptr->timed[TMD_OPP_POIS] && !redundant_timed_event(TMD_OPP_POIS)) resist_poison->setVisible(TRUE);
    else resist_poison->setVisible(FALSE);

    if (p_ptr->timed[TMD_FLYING]) flying->setVisible(TRUE);
    else flying->setVisible(FALSE);

    if (p_ptr->timed[TMD_NAT_LAVA] && !redundant_timed_event(TMD_NAT_LAVA)) native_lava->setVisible(TRUE);
    else native_lava->setVisible(FALSE);

    if (p_ptr->timed[TMD_NAT_OIL] && !redundant_timed_event(TMD_NAT_OIL)) native_oil->setVisible(TRUE);
    else native_oil->setVisible(FALSE);

    if (p_ptr->timed[TMD_NAT_SAND] && !redundant_timed_event(TMD_NAT_SAND)) native_sand->setVisible(TRUE);
    else native_sand->setVisible(FALSE);

    if (p_ptr->timed[TMD_NAT_TREE] && !redundant_timed_event(TMD_NAT_TREE)) native_tree->setVisible(TRUE);
    else native_tree->setVisible(FALSE);

    if (p_ptr->timed[TMD_NAT_WATER] && !redundant_timed_event(TMD_NAT_WATER)) native_water->setVisible(TRUE);
    else native_water->setVisible(FALSE);

    if (p_ptr->timed[TMD_NAT_MUD] && !redundant_timed_event(TMD_NAT_MUD)) native_mud->setVisible(TRUE);
    else native_mud->setVisible(FALSE);

    if ((p_ptr->timed[TMD_FAST] && !p_ptr->timed[TMD_SLOW]) || (!p_ptr->timed[TMD_FAST] && p_ptr->timed[TMD_SLOW]))
    {
        if (p_ptr->timed[TMD_FAST])
        {
            status_speed->setIcon(QIcon(":/icons/lib/icons/hasted.png"));
            status_speed->setToolTip("A temporary increase to player speed");
        }
        else  //TMD_SLOW
        {
            status_speed->setIcon(QIcon(":/icons/lib/icons/slowed.png"));
            status_speed->setToolTip("A temporary decrease to player speed");
        }

        status_speed->setVisible(TRUE);
    }
    else status_speed->setVisible(FALSE);

    if (p_ptr->timed[TMD_SLAY_ELEM]) elemental_weapon->setVisible(TRUE);
    else elemental_weapon->setVisible(FALSE);

    int feat = dungeon_info[p_ptr->py][p_ptr->px].feature_idx;
    u32b elem_flags1 = feat_ff3_match(feat, TERRAIN_MASK);

    if (elem_flags1)
    {
        if (is_player_native(p_ptr->py, p_ptr->px))
        {
            nativity->setIcon(QIcon(":/icons/lib/icons/native.png"));
            nativity->setToolTip("You are native to the elemental terrain in which you are standing.  Being native to a terrain can offer movement and combat bonuses, and protects the player from terrain damage.");
        }
        else  //Not native
        {
            nativity->setIcon(QIcon(":/icons/lib/icons/non-native.png"));
            QString nativity_message = QString("You are not native to the elemental terrain in which you are standing, which can impose movement and combat penalties.");
            if (f_info[feat].dam_non_native) nativity_message.append("  THE PLAYER IS TAKING DAMAGE FROM STANDING IN THIS TERRAIN!");
            nativity->setToolTip(nativity_message);
        }

        nativity->setVisible(TRUE);
    }
    else nativity->setVisible(FALSE);

    if (dungeon_info[p_ptr->py][p_ptr->px].cave_info & (CAVE_DTRAP))
    {
        if (dtrap_edge(p_ptr->py, p_ptr->px))
        {
            status_trap_detect->setIcon(QIcon(":/icons/lib/icons/dtrap_edge.png"));
            status_trap_detect->setToolTip("You are on the edge of an area where trap detection has been cast");
        }
        else
        {
            status_trap_detect->setIcon(QIcon(":/icons/lib/icons/dtrap_inside.png"));
            status_trap_detect->setToolTip("You are inside an area where trap detection has been cast");
        }

    }
    else
    {
        status_trap_detect->setIcon(QIcon(":/icons/lib/icons/dtrap_outside.png"));
        status_trap_detect->setToolTip("You are not in an area where trap detection has been cast");
    }
    status_trap_detect->setVisible(TRUE);
}

// Set up the statusbar.  The order of the icons needs
// to be consistent with the enum in qt_statusbar.h
void MainWindow::create_statusbar(void)
{
    status_bar = new QToolBar;
    status_bar->setObjectName("status_bar");
    addToolBar(Qt::BottomToolBarArea, status_bar);
    hide_statusbar();

    QString status = QString("The Word of Recall spell has been cast, and the player is about to be:<br><br>");
    status.append("1) If the player is in the dungeon, the player will be recalled back to town, or:<br><br>");
    status.append("2) If the player is in the town, the player will be recalled back to the dungeon recall depth.<br><br>");
    status.append( "This spell can be cancelled by casting another Word of Recall spell.");
    recall = new QAction(tr("Recall"), this);
    recall->setToolTip(status);
    recall->setIcon(QIcon(":/icons/lib/icons/recall.png"));
    status_bar->addAction(recall);
    recall->setVisible(FALSE);

    searching = new QAction(tr("Searching"), this);
    searching->setIcon(QIcon(":icons/lib/icons/search.png"));
    searching->setToolTip("Start searching.  This increases chance of finding hidden doors or traps, but slows the player down.");
    connect(searching, SIGNAL(triggered()), this, SLOT(toggle_searching()));
    status_bar->addAction(searching);
    searching->setVisible(FALSE);

    status_cut = new QAction(tr("Cut"), this);
    status_cut->setIcon(QIcon(":/icons/lib/icons/cut.png"));
    status_bar->addAction(status_cut);
    status_cut->setVisible(FALSE);

    status_stun = new QAction(tr("Stun"), this);
    status_stun->setIcon(QIcon(":/icons/lib/icons/stun.png"));
    status_bar->addAction(status_stun);
    status_stun->setVisible(FALSE);

    status_hunger = new QAction(tr("Food Status"), this);
    status_hunger->setIcon(QIcon(":/icons/lib/icons/hunger.png"));
    status_bar->addAction(status_hunger);
    status_hunger->setVisible(FALSE);

    study = new QAction(tr("Study"), this);
    study->setIcon(QIcon(":/icons/lib/icons/study.png"));
    connect(study, SIGNAL(triggered()), this, SLOT(click_study()));
    status_bar->addAction(study);
    study->setVisible(FALSE);

    blind = new QAction(tr("Blind"), this);
    blind->setIcon(QIcon(":/icons/lib/icons/blind.png"));
    blind->setToolTip("You are temporarily blind.  In addition to losing your sight, blindness also prevents spellcasting and reading scrolls, and reduces your chances to disarm traps or unlock doors.");
    status_bar->addAction(blind);
    blind->setVisible(FALSE);

    paralyzed = new QAction(tr("Paralyzed"), this);
    paralyzed->setIcon(QIcon(":/icons/lib/icons/paralyzed.png"));
    paralyzed->setToolTip("You are temporarily paralyzed.  You cannot move or act until the paralysis is over.");
    status_bar->addAction(paralyzed);
    paralyzed->setVisible(FALSE);

    confused = new QAction(tr("Confused"), this);
    confused->setIcon(QIcon(":/icons/lib/icons/confused.png"));
    confused->setToolTip("You are temporarily confused, which causes you to move in random directions, it also prevents running, spellcasting, reading scrolls, and reduces your chances sucessfully use wands, staves, and rods, and to disarm traps or unlock doors.");
    status_bar->addAction(confused);
    confused->setVisible(FALSE);

    afraid = new QAction(tr("Afraid"), this);
    afraid->setIcon(QIcon(":/icons/lib/icons/afraid.png"));
    afraid->setToolTip("You are temporarily afraid, which prevents you from attacking monsters with your weapon.");
    status_bar->addAction(afraid);
    afraid->setVisible(FALSE);

    hallucination = new QAction(tr("Hallucinating"), this);
    hallucination->setIcon(QIcon(":/icons/lib/icons/hallucination.png"));
    hallucination->setToolTip("You are temporarily hallucinating.  In addition to seeing random images, it also prevents spellcasting and reading scrolls, and reduces your chances sucessfully use wands, staves, and rods, and to disarm traps or unlock doors.");
    status_bar->addAction(hallucination);
    hallucination->setVisible(FALSE);

    poisoned = new QAction(tr("Poisoned"), this);
    poisoned->setIcon(QIcon(":/icons/lib/icons/poisoned.png"));
    poisoned->setToolTip("You are temporarily poisoned, which causes the player to take 1 hp damage per turn at regular speed, and prevents hit point regeneration.");
    status_bar->addAction(poisoned);
    poisoned->setVisible(FALSE);

    protect_evil = new QAction(tr("Protection From Evil"), this);
    protect_evil->setIcon(QIcon(":/icons/lib/icons/protection_evil.png"));
    protect_evil->setToolTip("You are temporarily protected from evil, which gives the player a saving throw from physical attacks from evil creatures of a lessor level than the player.");
    status_bar->addAction(protect_evil);
    protect_evil->setVisible(FALSE);

    invulnerability = new QAction(tr("Invulnerability"), this);
    invulnerability->setIcon(QIcon(":/icons/lib/icons/invulnerability.png"));
    invulnerability->setToolTip("You are temporarily invulnerable, which greatly increases the player's armor class, and prevents damage from physical attacks.  The player can still suffer side effects from physical attacks (e.g poison).");
    status_bar->addAction(invulnerability);
    invulnerability->setVisible(FALSE);

    hero = new QAction(tr("Heroism"), this);
    hero->setIcon(QIcon(":/icons/lib/icons/hero.png"));
    hero->setToolTip("You temporarily are heroic, which gives the player a bonus to-hit, 10 temporary extra hitpoints, and prevents the player from becoming afraid.");
    status_bar->addAction(hero);
    hero->setVisible(FALSE);

    berzerk = new QAction(tr("Berzerk Rage"), this);
    berzerk->setIcon(QIcon(":/icons/lib/icons/berzerk.png"));
    berzerk->setToolTip("You are temporarily in a state of Berzerk Rage, which gives the player a bonus to-hit and reduced armor class, 15 temporary extra hitpoints, and prevents the player from becoming afraid.");
    status_bar->addAction(berzerk);
    berzerk->setVisible(FALSE);

    shield = new QAction(tr("Shield"), this);
    shield->setIcon(QIcon(":/icons/lib/icons/shield.png"));
    shield->setToolTip("You are temporarily protected by the shield spell, which increases the player's armor class by 50.");
    status_bar->addAction(shield);
    shield->setVisible(FALSE);

    blessed = new QAction(tr("Blessed"), this);
    blessed->setIcon(QIcon(":/icons/lib/icons/bless.png"));
    shield->setToolTip("You are temporarily blessed, which increases the player's armor class by 5, and to-hit by 10.");
    status_bar->addAction(blessed);
    blessed->setVisible(FALSE);

    see_invisible = new QAction(tr("See Invisible"), this);
    see_invisible->setIcon(QIcon(":/icons/lib/icons/see_invisible.png"));
    see_invisible->setToolTip("You are temporarily able to see invisible creatures within line-of-sight.");
    status_bar->addAction(see_invisible);
    see_invisible->setVisible(FALSE);

    infravision = new QAction(tr("Infravision"), this);
    infravision->setIcon(QIcon(":/icons/lib/icons/infravision.png"));
    infravision->setToolTip("You temporarily have your infravision increased by 50 feet.");
    status_bar->addAction(infravision);
    infravision->setVisible(FALSE);

    resist_fire = new QAction(tr("Resist Fire"), this);
    resist_fire->setIcon(QIcon(":/icons/lib/icons/resist_fire.png"));
    resist_fire->setToolTip("You temporarily resist fire, which protects inventory and reduces damage from fire attacks.<br>Offers cumulative protection if the player has a permanent source of resist.");
    status_bar->addAction(resist_fire);
    resist_fire->setVisible(FALSE);

    resist_cold = new QAction(tr("Resist Cold"), this);
    resist_cold->setIcon(QIcon(":/icons/lib/icons/resist_cold.png"));
    resist_cold->setToolTip("You temporarily resist cold, which protects inventory and reduces damage from cold attacks.<br>Offers cumulative protection if the player has a permanent source of resist.");
    status_bar->addAction(resist_cold);
    resist_cold->setVisible(FALSE);

    resist_acid = new QAction(tr("Resist Acid"), this);
    resist_acid->setIcon(QIcon(":/icons/lib/icons/resist_acid.png"));
    resist_acid->setToolTip("You temporarily resist acid, which protects inventory and reduces damage from acid attacks.<br>Offers cumulative protection if the player has a permanent source of resist.");
    status_bar->addAction(resist_acid);
    resist_acid->setVisible(FALSE);

    resist_lightning = new QAction(tr("Resist Lightning"), this);
    resist_lightning->setIcon(QIcon(":/icons/lib/icons/resist_lightning.png"));
    resist_lightning->setToolTip("You temporarily resist lightning, which protects inventory and reduces damage from lightning attacks.<br>Offers cumulative protection if the player has a permanent source of resist.");
    status_bar->addAction(resist_lightning);
    resist_lightning->setVisible(FALSE);

    resist_poison = new QAction(tr("Resist Poison"), this);
    resist_poison->setIcon(QIcon(":/icons/lib/icons/resist_poison.png"));
    resist_poison->setToolTip("You temporarily resist poison, which protects damage from poison attacks, and prevents the player from being poisoned.<br>Offers cumulative protection if the player has a permanent source of resist.");
    status_bar->addAction(resist_poison);
    resist_poison->setVisible(FALSE);

    flying = new QAction(tr("Flying"), this);
    flying->setIcon(QIcon(":/icons/lib/icons/flying.png"));
    flying->setToolTip("You are temporarily flying, which allows the player to go over dangerous terrain without suffering damage.<br>Also, the player can go over traps without setting them off.  Negates any player combat or monement bonuses or penalties from terrain.");
    status_bar->addAction(flying);
    flying->setVisible(FALSE);

    native_lava = new QAction(tr("Lava Nativity"), this);
    native_lava->setIcon(QIcon(":/icons/lib/icons/native_lava.png"));
    native_lava->setToolTip("You temporarily native to lava, which provides movement and combat bonuses when the player is in lava, and allows the player to safely pass through lava based terrain.  Also reduces damage to player and inventory from lava attacks.");
    status_bar->addAction(native_lava);
    native_lava->setVisible(FALSE);

    native_oil = new QAction(tr("Oil Nativity"), this);
    native_oil->setIcon(QIcon(":/icons/lib/icons/native_oil.png"));
    native_oil->setToolTip("You temporarily native to oil, which provides movement and combat bonuses when the player is in lava, and allows the player to safely pass through lava based terrain.  Also reduces damage to player and inventory from lava attacks.");
    status_bar->addAction(native_oil);
    native_oil->setVisible(FALSE);

    native_sand = new QAction(tr("Sand Nativity"), this);
    native_sand->setIcon(QIcon(":/icons/lib/icons/native_sand.png"));
    native_sand->setToolTip("You temporarily native to sand, which provides  movement and combat bonuses when the player is in sand.  Also reduces damage to player and inventory from sand attacks.");
    status_bar->addAction(native_sand);
    native_sand->setVisible(FALSE);

    native_tree = new QAction(tr("Forest Nativity"), this);
    native_tree->setIcon(QIcon(":/icons/lib/icons/native_tree.png"));
    native_tree->setToolTip("You temporarily native to the forest, which provides  movement and combat bonuses when the player is in forest, and allows the player to safely pass through forest based terrain.");
    status_bar->addAction(native_tree);
    native_tree->setVisible(FALSE);

    native_water = new QAction(tr("Water Nativity"), this);
    native_water->setIcon(QIcon(":/icons/lib/icons/native_water.png"));
    native_water->setToolTip("You temporarily native to water, which provides  movement and combat bonuses when the player is in water.  Also reduces damage to player and inventory from water attacks.");
    status_bar->addAction(native_water);
    native_water->setVisible(FALSE);

    native_mud = new QAction(tr("Mud Nativity"), this);
    native_mud->setIcon(QIcon(":/icons/lib/icons/native_mud.png"));
    native_mud->setToolTip("You temporarily native to mud, which provides  movement and combat bonuses when the player is in mud.  Also reduces damage to player and inventory from mud attacks.");
    status_bar->addAction(native_mud);
    native_mud->setVisible(FALSE);

    status_speed = new QAction(tr("Temporary Speed"), this);
    status_speed->setIcon(QIcon(":/icons/lib/icons/hasted.png"));
    status_bar->addAction(status_speed);
    status_speed->setVisible(FALSE);

    elemental_weapon = new QAction(tr("Elemental Weapon"), this);
    elemental_weapon->setIcon(QIcon(":/icons/lib/icons/elemental_weapon.png"));
    elemental_weapon->setToolTip("You temporarily have an elemental weapon, which provides the player's weapon a temporary fire, cold, acid, lightning, and poison brand.  This effectively doubles weapon base damage unless the monster resists all five elements.");
    status_bar->addAction(elemental_weapon);
    elemental_weapon->setVisible(FALSE);

    call_hourns = new QAction(tr("Call Hourns"), this);
    call_hourns->setIcon(QIcon(":/icons/lib/icons/hourns.png"));
    call_hourns->setToolTip("The druid incantantation Call Hourns is active.  Nearly trees will come alive and attack nearby monsters.");
    status_bar->addAction(call_hourns);
    call_hourns->setVisible(FALSE);

    nativity = new QAction(tr("Native Status"), this);
    nativity->setIcon(QIcon(":/icons/lib/icons/native.png"));
    status_bar->addAction(nativity);
    nativity->setVisible(FALSE);

    status_trap_detect = new QAction(tr("Trap Detection Status"), this);
    status_trap_detect->setIcon(QIcon(":/icons/lib/icons/dtrap_inside.png"));
    status_bar->addAction(status_trap_detect);
    status_trap_detect->setVisible(FALSE);

}


void MainWindow::hide_statusbar(void)
{
    if (status_bar == NULL) return;
    status_bar->setVisible(FALSE);
}

void MainWindow::show_statusbar(void)
{
    status_bar->setVisible(TRUE);
}

