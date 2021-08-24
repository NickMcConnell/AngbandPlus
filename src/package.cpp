
/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "package.h"
#include "npp.h"

Package::Package(QString _path)
{
    path = _path;
    data_pos = 0;
    fp = new QFile(path);
    if (!fp->open(QFile::ReadOnly)) return;

    // Read the header
    while (true)
    {
        QString line = read_line();
        if (line == "") break;  // Header ends with an empty line

        PackageItem item;
        item.name = line;

        line = read_line();
        if (line == "")
        {   // Paranoia
            fp->close();
            items.clear();
            return;
        }
        item.size = line.toInt();

        items.append(item);
    }

    // Save the position of the data
    data_pos = fp->pos();
}

qint64 Package::item_position(QString name)
{
    // Start from the data mark
    qint64 pos = data_pos;

    for (int i = 0; i < items.size(); i++)
    {
        if (items.at(i).name == name) return pos;
        else pos += items.at(i).size;
    }

    return -1;
}

qint64 Package::item_size(QString name)
{
    for (int i = 0; i < items.size(); i++)
    {
        if (items.at(i).name == name) return items.at(i).size;
    }

    return -1;
}

QByteArray Package::get_item(QString name)
{
    QByteArray data;

    qint64 pos = item_position(name);

    if (pos == -1) return data;

    // Jump to the item location
    if (!fp->seek(pos)) return data;

    qint64 size = item_size(name);

    char buf[size];

    // Read the item
    int n = fp->read(buf, size);

    if (n != size) return data;

    return QByteArray(buf, size);
}

int Package::extract_to(QString folder)
{
    int n = 0;

    for (int i = 0; i < items.size(); i++)
    {
        QString name = items.at(i).name;
        QByteArray data = get_item(name);
        QDir dir(folder);
        QString path = dir.absoluteFilePath(items.at(i).name);
        QFile out(path);
        if (out.open(QFile::WriteOnly))
        {
            out.write(data.data(), data.size());
            out.close();
            ++n;
        }
    }

    return n;
}

Package::~Package()
{
    if (fp->isOpen()) fp->close();
    delete fp;
    fp = 0;
}

QString Package::read_line()
{
    if (!is_open()) return "";

    char buf[2048];
    qint64 n = fp->readLine(buf, sizeof(buf));

    if (n < 1) return "";

    QString line(buf);

    return line.trimmed(); // Nuke the \n
}

bool Package::is_open()
{
    return fp->isOpen();
}

int create_package(QString name, QString folder, QString ext)
{
    QDir dir(folder);
    dir.setFilter(QDir::Files | QDir::Readable);
    QFileInfoList items;

    int n = 0;

    QFileInfoList lst = dir.entryInfoList();
    for (int i = 0; i < lst.size(); i++)
    {
        QFileInfo info = lst.at(i);
        if (info.fileName().endsWith(ext))
        {
            items.append(info);
        }
    }

    QFile out(name);
    if (!out.open(QFile::WriteOnly))
    {
        return 0;
    }

    for (int i = 0; i < items.size(); i++)
    {
        QString name = items.at(i).fileName().trimmed();
        name.append("\n");
        out.write(name.toLatin1());

        QString size = QString::number(items.at(i).size()).trimmed();
        size.append("\n");
        out.write(size.toLatin1());
    }
    out.write("\n"); // End the header with an empty line

    for (int i = 0; i < items.size(); i++)
    {
        QFile in(items.at(i).absoluteFilePath());
        if (!in.open(QFile::ReadOnly))
        {
            out.close();
            return 0;
        }

        QByteArray data = in.readAll();
        out.write(data);

        in.close();

        ++n;
    }

    out.close();

    return n;
}
