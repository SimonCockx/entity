# Generated by Django 3.1.4 on 2020-12-28 17:40

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('app', '0001_initial'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='eventorm',
            name='time',
        ),
        migrations.AddField(
            model_name='eventorm',
            name='year',
            field=models.IntegerField(default=2020),
            preserve_default=False,
        ),
    ]
