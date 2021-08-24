#ifndef PACKAGE_H
#define PACKAGE_H

#include <QtCore>
#include <QList>

class PackageItem
{
public:
    QString name;
    qint64 size;
};

class Package
{
public:
    QFile *fp;
    QString path;
    qint64 data_pos;
    QList<PackageItem> items;

    Package(QString _path);

    QString read_line();

    bool is_open();

    QByteArray get_item(QString name);         // THIS IS ALL WE NEED

    qint64 item_position(QString name);

    qint64 item_size(QString name);

    int extract_to(QString folder);          // For later tile manipulation

    virtual ~Package();
};

// Create a package containing all the files ending with "ext"
// The files are read from "folder"
int create_package(QString name, QString folder, QString ext = ".png");

#endif // PACKAGE_H
