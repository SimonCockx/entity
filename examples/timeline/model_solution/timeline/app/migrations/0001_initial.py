# Generated by Django 3.1.4 on 2020-12-27 16:40

import app.entities
from django.db import migrations, models


class Migration(migrations.Migration):

    initial = True

    dependencies = [
    ]

    operations = [
        migrations.CreateModel(
            name='EventORM',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(max_length=100)),
                ('time', models.DateTimeField()),
                ('description', models.TextField()),
            ],
            bases=(models.Model, app.entities.Event),
        ),
    ]
