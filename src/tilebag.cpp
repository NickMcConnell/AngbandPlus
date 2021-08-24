/* File: qt_mainwindow.cpp */

/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */


#include "tilebag.h"
#include "package.h"
#include "npp.h"
#include "src/init.h"
#include <src/knowledge.h>
#include <QVBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QToolButton>
#include <QPushButton>
#include <QFileDialog>

TileBag *tiles_64x64;
TileBag *tiles_32x32;
TileBag *tiles_8x8;
TileBag *tiles_flav_64x64;
TileBag *tiles_flav_32x32;
TileBag *tiles_flav_8x8;
TileBag *tiles_feat_64x64;
TileBag *tiles_feat_32x32;
TileBag *tiles_feat_8x8;
TileBag *tiles_projections;
TileBag *current_tiles;
TileBag *current_flav_tiles;
TileBag *current_feat_tiles;


void init_tile_bags()
{
    if (tiles_64x64) delete tiles_64x64;
    tiles_64x64 = 0;
    if (tiles_32x32) delete tiles_32x32;
    tiles_32x32 = 0;
    if (tiles_8x8) delete tiles_8x8;
    tiles_8x8 = 0;
    if (tiles_flav_64x64) delete tiles_flav_64x64;
    tiles_flav_64x64 = 0;
    if (tiles_flav_32x32) delete tiles_flav_32x32;
    tiles_flav_32x32 = 0;
    if (tiles_8x8) delete tiles_flav_8x8;
    tiles_flav_8x8 = 0;
    if (tiles_feat_64x64) delete tiles_feat_64x64;
    tiles_feat_64x64 = 0;
    if (tiles_feat_32x32) delete tiles_feat_32x32;
    tiles_feat_32x32 = 0;
    if (tiles_feat_8x8) delete tiles_feat_8x8;
    tiles_feat_8x8 = 0;

    if (!tiles_projections) tiles_projections = new TileBag(npp_dir_graf.absoluteFilePath("projections.pak"));

    QString pak64("tiles_64x64.pak");
    QString pak32("tiles_32x32.pak");
    QString pak8("tiles_8x8.pak");
    QString pakflav64("tiles_flav_64x64.pak");
    QString pakflav32("tiles_flav_32x32.pak");
    QString pakflav8("tiles_flav_8x8.pak");
    QString pakfeat64("tiles_feat_64x64.pak");
    QString pakfeat32("tiles_feat_32x32.pak");
    QString pakfeat8("tiles_feat_8x8.pak");
    tiles_64x64 = new TileBag(npp_dir_graf.absoluteFilePath(pak64));
    tiles_32x32 = new TileBag(npp_dir_graf.absoluteFilePath(pak32));
    tiles_8x8 = new TileBag(npp_dir_graf.absoluteFilePath(pak8));
    tiles_flav_64x64 = new TileBag(npp_dir_graf.absoluteFilePath(pakflav64));
    tiles_flav_32x32 = new TileBag(npp_dir_graf.absoluteFilePath(pakflav32));
    tiles_flav_8x8 = new TileBag(npp_dir_graf.absoluteFilePath(pakflav8));
    tiles_feat_64x64 = new TileBag(npp_dir_graf.absoluteFilePath(pakfeat64));
    tiles_feat_32x32 = new TileBag(npp_dir_graf.absoluteFilePath(pakfeat32));
    tiles_feat_8x8 = new TileBag(npp_dir_graf.absoluteFilePath(pakfeat8));

    if (use_graphics == GRAPHICS_RAYMOND_GAUSTADNES)
    {
        current_tiles = tiles_64x64;
        current_flav_tiles = tiles_flav_64x64;
        current_feat_tiles = tiles_feat_64x64;
    }
    else if (use_graphics == GRAPHICS_DAVID_GERVAIS)
    {
        current_tiles = tiles_32x32;
        current_flav_tiles = tiles_flav_32x32;
        current_feat_tiles = tiles_feat_32x32;
    }
    else if (use_graphics == GRAPHICS_ORIGINAL)
    {
        current_tiles = tiles_8x8;
        current_flav_tiles = tiles_flav_8x8;
        current_feat_tiles = tiles_feat_8x8;
    }
    else
    {
        current_tiles = 0;
        current_flav_tiles = 0;
        current_feat_tiles = 0;
    }
}

TileBag::TileBag(QString path)
{
    pak = new Package(path);
    if (!pak->is_open())
    {
        pop_up_message_box(QString("Couldn't load %1").arg(path), QMessageBox::Critical);
    }
}

bool TileBag::has_tile(QString name)
{
    if (!pak->is_open()) return false;
    QString ext(".png");
    if (!name.endsWith(ext)) name += ext;
    if (pak->item_position(name) < 0) return false;
    return true;
}

QPixmap TileBag::get_tile(QString name)
{
    if (!pak->is_open()) return ui_make_blank();

    if (!name.endsWith(".png")) name += ".png";

    if (cache.contains(name)) // Cache hit?
    {
        return cache.value(name);
    }

    QByteArray data = pak->get_item(name);  // Get png data from package
    if (data.size() < 1)
    {
        color_message("Tile not found: " + name, TERM_ORANGE);
        return ui_make_blank(); // Check existence of the tile
    }

    QImage img;
    img.loadFromData(data);                 // Convert png data to RGB

    QPixmap pix = QPixmap::fromImage(img);

    cache.insert(name, pix);                // Save for later use

    return pix;
}

void TileBag::clear_cache()
{
    cache.clear();
}

TileBag::~TileBag()
{
    delete pak;
}

static QString tile_obj_name_convert(QString orig_name)
{
    // first, make it standard ASCII, then lowercase
    orig_name = to_ascii(orig_name);
    orig_name = orig_name.toLower();

    QString star_string = "*";

    bool contains_star = orig_name.contains(star_string);

    //now delete all commas, and then replace spaces with '_', then start with "obj_".
    orig_name.remove(QChar(','));
    orig_name.remove(QString("'"));
    orig_name.remove(QChar('['));
    orig_name.remove(QChar(']'));
    orig_name.remove(QChar('('));
    orig_name.remove(QChar(')'));
    orig_name.remove(QChar('*'));
    orig_name.replace(QChar('/'), QChar('_'));
    orig_name.replace(QChar(' '), QChar('_'));
    orig_name.prepend("obj_");
    if (contains_star) orig_name.append("_star");
    return (orig_name);
}


static QString tile_mon_name_convert(QString orig_name)
{
    // first, make it standard ASCII, then lowercase
    orig_name = to_ascii(orig_name);
    orig_name = orig_name.toLower();

    //now delete all commas, and then replace spaces with '_', then start with "mon_".
    orig_name.remove(QChar(','));
    orig_name.remove(QString("'"));
    orig_name.replace(QChar(' '), QChar('_'));
    orig_name.prepend("mon_");
    return (orig_name);
}

static QString tile_feat_name_convert(QString orig_name)
{
    // first, make it standard ASCII, then lowercase
    orig_name = to_ascii(orig_name);
    orig_name = orig_name.toLower();

    //now delete all commas, and then replace spaces with '_', then start with "mon_".
    orig_name.remove(QChar(','));
    orig_name.remove(QChar('('));
    orig_name.remove(QChar(')'));
    orig_name.remove(QString("'"));
    orig_name.replace(QChar(' '), QChar('_'));
    orig_name.prepend("feat_");
    return (orig_name);
}

static QString tile_flav_name_convert(QString orig_name, int tval)
{
    // first, make it standard ASCII, then lowercase
    orig_name = to_ascii(orig_name);
    orig_name = orig_name.toLower();

    //now delete all commas, and then replace spaces with '_', then start with "mon_".
    orig_name.remove(QChar(','));
    orig_name.remove(QChar('('));
    orig_name.remove(QChar(')'));
    orig_name.remove(QString("'"));
    orig_name.remove(QChar('*'));
    orig_name.replace(QChar('/'), QChar('_'));
    orig_name.replace(QChar(' '), QChar('_'));
    if (tval == TV_RING) orig_name.prepend("ring_");
    else if (tval == TV_AMULET) orig_name.prepend("amulet_");
    else if (tval == TV_STAFF) orig_name.prepend("staff_");
    else if (tval == TV_WAND) orig_name.prepend("wand_");
    else if (tval == TV_ROD) orig_name.prepend("rod_");
    else if (tval == TV_FOOD) orig_name.prepend("mushroom_");
    else if (tval == TV_POTION) orig_name.prepend("potion_");
    else if (tval == TV_SCROLL) orig_name.prepend("scroll_");
    orig_name.prepend("flav_");
    return (orig_name);
}


void extract_tiles(void)
{
    int i;

    for (i = 1; i < z_info->r_max; i++)
    {
        monster_race *r_ptr = &r_info[i];
        if (r_ptr->r_name_full.isEmpty()) continue;
        r_ptr->tile_id = tile_mon_name_convert(monster_desc_race(i));

        // Hunt for the double height tiles
        r_ptr->double_height_tile = FALSE;

        if (use_graphics == GRAPHICS_RAYMOND_GAUSTADNES)
        {
            QPixmap this_pixmap = current_tiles->get_tile(r_ptr->tile_id);
            if (this_pixmap.height() >= this_pixmap.width()*9/5) r_ptr->double_height_tile = TRUE;
        }
    }
    for (i = 0; i < z_info->k_max; i++)
    {
        object_kind *k_ptr = &k_info[i];
        if (k_ptr->k_name.isEmpty()) continue;
        object_type object_type_body;
        object_type *o_ptr = &object_type_body;
        /* Check for known artifacts, display them as artifacts */
        if (k_ptr->k_flags3 & (TR3_INSTA_ART))
        {
            int art_num = i;
            if (game_mode == GAME_NPPANGBAND)
            {
                if (k_ptr->tval == TV_HAFTED && k_ptr->sval == SV_GROND) art_num = ART_GROND;
                if (k_ptr->tval == TV_CROWN && k_ptr->sval == SV_MORGOTH) art_num = ART_MORGOTH;
            }
            make_fake_artifact(o_ptr, art_num);
        }
        else
        {
            o_ptr->object_wipe();
            object_prep(o_ptr, i);
            apply_magic_fake(o_ptr);
            o_ptr->ident |= (IDENT_STORE);
            if (!k_info[i].flavor)
            {
                /* Mark the item as fully known */
                o_ptr->mark_fully_known(FALSE);
                o_ptr->ident |= (IDENT_STORE);
            }
        }

        QString object_name;
        if (o_ptr->tval == TV_GOLD) object_name = strip_name(i);
        else object_name = object_desc(o_ptr, ODESC_BASE);
        k_ptr->tile_id = tile_obj_name_convert(object_name);
    }
    for (i = 0; i < z_info->f_max; i++)
    {
        feature_type *f_ptr = &f_info[i];
        if (f_ptr->f_name.isEmpty()) continue;
        QString feat_name = feature_desc(i, FALSE, FALSE);
        f_ptr->tile_id = tile_feat_name_convert(feat_name);
    }
    for (i = 0; i < z_info->flavor_max; i++)
    {
        flavor_type *flavor_ptr = &flavor_info[i];
        if (flavor_ptr->text.isEmpty() && flavor_ptr->tval != TV_SCROLL) continue;
        QString flavor_name = flavor_ptr->text;
        flavor_ptr->tile_id = tile_flav_name_convert(flavor_name, flavor_ptr->tval);
    }

    QString race_name = to_ascii(p_info[p_ptr->prace].pr_name).toLower();
    QString class_name = to_ascii(c_info[p_ptr->pclass].cl_name).toLower();

    if (use_graphics == GRAPHICS_RAYMOND_GAUSTADNES)
    {
        p_ptr->tile_id = QString("player_%1").arg(race_name);
        if (p_ptr->psex == SEX_FEMALE) p_ptr->tile_id.append("_female");
        else p_ptr->tile_id.append("_male");
        p_ptr->tile_id.append(QString("_%1").arg(class_name));
    }
    else p_ptr->tile_id = QString("player_%1_%2").arg(race_name).arg(class_name);
}


PackageDialog::PackageDialog(QString _mode)
{
    mode = _mode;

    QVBoxLayout *lay1 = new QVBoxLayout;

    QWidget *area2 = new QWidget;
    lay1->addWidget(area2);
    QGridLayout *lay2 = new QGridLayout;
    lay2->setContentsMargins(0, 0, 0, 0);
    lay2->setColumnStretch(1, 1);
    area2->setLayout(lay2);

    int row = 0;

    QLabel *lb = new QLabel;
    if (mode == "create")
    {
        lb->setText("Create a tile package");
    }
    else
    {
        lb->setText("Extract tiles from a package");
    }
    lb->setStyleSheet("font-size: 1.5em; font-weight: bold;");
    lay2->addWidget(lb, row, 0, 1, 3);

    ++row;

    lay2->addWidget(new QLabel(tr("Package")), row, 0);

    pak_path = new QLineEdit;
    pak_path->setReadOnly(true);
    lay2->addWidget(pak_path, row, 1);

    QToolButton *btn2 = new QToolButton();
    btn2->setText("...");
    //btn2->setSizePolicy(QSizePolicy::Maximum, QSizePolicy::Fixed);
    lay2->addWidget(btn2, row, 2);
    connect(btn2, SIGNAL(clicked()), this, SLOT(find_pak()));

    ++row;

    lay2->addWidget(new QLabel(tr("Tiles folder")), row, 0);

    folder_path = new QLineEdit;
    folder_path->setReadOnly(true);
    lay2->addWidget(folder_path, row, 1);

    QToolButton *btn3 = new QToolButton();
    btn3->setText("...");
    //btn3->setSizePolicy(QSizePolicy::Maximum, QSizePolicy::Fixed);
    lay2->addWidget(btn3, row, 2);
    connect(btn3, SIGNAL(clicked()), this, SLOT(find_folder()));

    ++row;

    QSpacerItem *spacer = new QSpacerItem(1, 1, QSizePolicy::Fixed, QSizePolicy::Expanding);
    lay2->addItem(spacer, row, 0);

    QWidget *area3 = new QWidget;
    lay1->addWidget(area3);
    QHBoxLayout *lay3 = new QHBoxLayout;
    area3->setLayout(lay3);
    lay3->setContentsMargins(0, 0, 0, 0);

    spacer = new QSpacerItem(1, 1, QSizePolicy::Expanding, QSizePolicy::Fixed);
    lay3->addItem(spacer);

    QPushButton *btn1 = new QPushButton(tr("Go!"));
    lay3->addWidget(btn1);
    connect(btn1, SIGNAL(clicked()), this, SLOT(do_accept()));

    QPushButton *btn4 = new QPushButton(tr("Close"));
    lay3->addWidget(btn4);
    connect(btn4, SIGNAL(clicked()), this, SLOT(reject()));

    setLayout(lay1);

    this->exec();
}

void PackageDialog::do_accept()
{
    if (pak_path->text().isEmpty() || folder_path->text().isEmpty())
    {
        pop_up_message_box(tr("Complete both fields"), QMessageBox::Critical);
        return;
    }

    if (mode == "create")
    {
        int n = create_package(pak_path->text(), folder_path->text());
        pop_up_message_box(tr("Imported tiles: %1").arg(n));
    }
    else
    {
        Package pak(pak_path->text());
        if (!pak.is_open())
        {
            pop_up_message_box(tr("Couldn't load the package"), QMessageBox::Critical);
        }
        else
        {
            int n = pak.extract_to(folder_path->text());
            pop_up_message_box(tr("Extracted tiles: %1").arg(n));
        }
    }
}

void PackageDialog::find_pak()
{
    QString path;

    if (mode == "extract")
    {
        path = QFileDialog::getOpenFileName(this, tr("Select a package"), npp_dir_graf.path(),
                                            tr("Packages (*.pak)"));
    }
    else
    {
        path = QFileDialog::getSaveFileName(this, tr("Select a package"), npp_dir_graf.path(),
                                            tr("Packages (*.pak)"));
    }

    if (path != "") pak_path->setText(path);
}

void PackageDialog::find_folder()
{
    QString path;

    path = QFileDialog::getExistingDirectory(this, tr("Select a folder"), npp_dir_graf.path());

    if (path != "") folder_path->setText(path);
}



#ifdef UNDEFINED

// Useful code for extracting tiles from a tilemap
class tile_coords
{
public:
    QString name;
    int y_coord;
    int x_coord;
};

static tile_coords these_tiles[] =
{
    {"1  Phial~",7,29},
    {"2  Star~",7,30},



// Null line is needed to stop the loop
    {NULL, 0, 0},
};

void get_8x8_tiles(void)
{
    QPixmap tileset(":/icons/lib/graf/npp8x8.png");

    for (int i = 0; i < 2000; i++)
    {
        tile_coords *this_tile = &these_tiles[i];
        if (this_tile->name.isNull()) break;
        QPixmap tile_bit = tileset.copy(this_tile->x_coord*8, this_tile->y_coord*8, 8, 8);
        QString file_name = npp_dir_graf.absoluteFilePath(this_tile->name);
        file_name.append(".png");

        tile_bit.save(file_name);

    }


}

#endif // NOT_DEFINED
