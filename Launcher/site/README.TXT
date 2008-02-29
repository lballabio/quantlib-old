
Launcher\site\README.TXT
========================

Before using the Launcher you need to configure the data specific to your site.

Create directory Launcher\site\data.

Copy this file:
    Launcher\site\config.template.xml
To:
    Launcher\site\data\config.xml

And edit the file to reflect the parameters for your site.

You also have to edit Launcher\site\users.template.xml and include the users
of your site. This file will have to be pointed in confign.xml by the element
<UserConfigurationFile>\\servername\path\to\users.xml</UserConfigurationFile>
