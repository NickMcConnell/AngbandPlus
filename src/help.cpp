
/*
 * Copyright (c) 2015 Jeff Greene, Diego Gonzalez
 *
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */


#include <src/help.h>
#include <QFile>
#include <QTextStream>
#include <src/init.h>

QString get_help_topic(QString help_file, QString help_topic)
{
    bool use_text = FALSE;

    QString return_string;
    return_string.clear();

    help_file.prepend(QString("%1/") .arg(npp_dir_help.path()));
    help_file.append(".txt");

    QFile topic_file;
    topic_file.setFileName(help_file);

    /* Handle file failure */
    if (!topic_file.open(QIODevice::ReadOnly))
    {
        return (" ");
    }

    QTextStream text_in(&topic_file);
    while (!text_in.atEnd())
    {
        QString line = text_in.readLine();

        if (line.contains(QString("*****")))
        {
            line.remove(QChar('*'));
            // We are at the end of the topic.. Quit appending
            if (line.contains(QString("<end>"), Qt::CaseInsensitive))
            {
                if (use_text) break;
            }

            // We found the topic.  Begin appending.
            if (line.contains(help_topic,  Qt::CaseInsensitive))
            {
                use_text = TRUE;
            }
        }
        // Add the text if we are mid-topic
        if (use_text)
        {
            // bold the topic
            if (!return_string.length()) return_string.append(QString("<b>%1</b>") .arg(line));
            else return_string.append(line);
            return_string.append("<br>");
        }
    }

    topic_file.close();

    // We never found a topic
    if (!return_string.length()) return(QString("topic not found"));

    return (return_string);
}
