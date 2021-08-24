#ifndef WIZARD_MODE_H
#define WIZARD_MODE_H

#include <QComboBox>
#include <QDialogButtonBox>

#include "src/npp.h"

class EditObjectDialog : public QDialog
{
    Q_OBJECT

public:
    explicit EditObjectDialog(void);
};


class EditCharacterDialog : public QDialog
{
    Q_OBJECT

public:
    explicit EditCharacterDialog(void);
};

class MakeArtifactDialog : public QDialog
{
    Q_OBJECT

public:
    explicit MakeArtifactDialog(void);

private slots:
    void update_art_choice(int choice);

private:

    QComboBox *art_choice;
    QString get_artifact_display_name(int a_idx);

    int art_num;
};

class MakeObjectDialog : public QDialog
{
    Q_OBJECT

public:
    explicit MakeObjectDialog(void);

private slots:
    void update_obj_choice(int choice);

private:

    QComboBox *obj_choice;
    QString get_object_display_name(int o_idx);

    int obj_num;
};

class MakeMonsterDialog : public QDialog
{
    Q_OBJECT

public:
    explicit MakeMonsterDialog(void);

private slots:
    void update_mon_choice(int choice);

private:

    QComboBox *mon_choice;
    QString get_mon_display_name(int r_idx);

    int mon_num;
};

class MakeFeatureDialog : public QDialog
{
    Q_OBJECT

public:
    explicit MakeFeatureDialog(void);

private slots:
    void update_feat_choice(int choice);

private:

    QComboBox *feat_choice;
    QString get_feat_display_name(int f_idx);

    int feat_num;
};


class WizardModeDialog : public QDialog
{
    Q_OBJECT

public:
    explicit WizardModeDialog(void);

private:
    QLabel *main_prompt;
    QLabel *player_section;
    QLabel *dungeon_section;
    QLabel *object_section;

    QDialogButtonBox *buttons;



private slots:

    //Wizard mode functions
    void wiz_cure_all(void);
    void wiz_know_all(void);
    void wiz_jump(void);
    void wiz_teleport_to_target(void);
    void wiz_phase_door(void);
    void wiz_teleport(void);
    void wiz_edit_character(void);
    void wiz_print_spoilers(void);
    void wiz_summon(void);
    void wiz_banish(void);
    void wiz_create_monster(void);
    void wiz_detect_all_monsters(void);
    void wiz_detection(void);
    void wiz_magic_mapping(void);
    void wiz_level_light(void);
    void wiz_redraw_dungeon(void);
    void wiz_create_feature(void);
    void wiz_mass_create_items(void);
    void wiz_create_good_item(void);
    void wiz_create_great_item(void);
    void wiz_edit_object(void);
    void wiz_mass_identify_items(void);
    void wiz_winners_kit(void);
    void wiz_create_artifact(void);
    void wiz_create_object(void);
};



#endif // WIZARD_MODE_H
