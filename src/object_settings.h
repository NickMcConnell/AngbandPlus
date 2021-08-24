#ifndef OBJECT_SETTINGS_H
#define OBJECT_SETTINGS_H

#include <QVBoxLayout>
#include <QDialogButtonBox>
#include <QButtonGroup>
#include <QRadioButton>
#include <QDialog>
#include <QLabel>
#include <src/object_classes.h>
#include <QPointer>

enum
{
    SETTINGS_FULL_OBJECT = 0,
    SETTINGS_OBJECT_KIND,
    SETTINGS_ARTIFACT,
};

class verify_data
{
public:
    int matching_command;
    QString box_label;
    QString box_tooltip;

};

class ObjectSettingsAux: public QDialog
{
    Q_OBJECT

public:
    void add_squelch_buttons(QVBoxLayout *squelch_buttons);
    void add_quality_buttons(QVBoxLayout *quality_buttons);
    void add_ego_buttons(QVBoxLayout *ego_buttons);

    QPointer<QButtonGroup> squelch_group;
    QPointer<QRadioButton> squelch_never;
    QPointer<QRadioButton> squelch_pickup_no;
    QPointer<QRadioButton> squelch_pickup_yes;
    QPointer<QRadioButton> squelch_always;

    object_kind *k_ptr;
    object_type *o_ptr;

    QPointer<QButtonGroup> quality_group;
    QPointer<QRadioButton> quality_none;
    QPointer<QRadioButton> quality_cursed;
    QPointer<QRadioButton> quality_average;
    QPointer<QRadioButton> quality_good_strong;
    QPointer<QRadioButton> quality_good_weak;
    QPointer<QRadioButton> quality_all_but_artifact;

    QPointer<QButtonGroup> ego_group;
    QPointer<QRadioButton> ego_no;
    QPointer<QRadioButton> ego_yes;

    byte squelch_type;

public slots:
    void update_squelch_setting(int id);
    void update_quality_setting(int id);
    void update_ego_setting(int id);
};


extern verify_data verification_data[VERIFY_MAX];

/*
 *  Include all generic functions to be
 * used across all object dialog boxes.
 */
class ObjectSettingsDialog: public ObjectSettingsAux
{
    Q_OBJECT
public:

    ObjectSettingsDialog(s16b o_idx, byte settings_mode);

private:


    QPointer<QVBoxLayout> object_type_ver;
    QPointer<QButtonGroup> object_type_group;

    QPointer<QVBoxLayout> object_kind_ver;
    QPointer<QButtonGroup> object_kind_group;

    bool do_object_type;
    bool do_object_kind;

    void add_type_checkbox(byte which_ver);
    void add_kind_checkbox(byte which_ver);
    void add_object_verifications(byte settings_mode);

private slots:
    void update_object_type_settings(int id, bool checked);
    void update_object_kind_settings(int id, bool checked);

};

// object_settings
extern void object_settings(s16b o_idx);
extern void object_kind_settings(s16b k_idx);
extern void object_artifact_settings(s16b a_idx);
extern bool get_item_allow(int item, int verify_command);
extern void apply_object_kind_settings(object_type *o_ptr);


#endif // OBJECT_SETTINGS_H
