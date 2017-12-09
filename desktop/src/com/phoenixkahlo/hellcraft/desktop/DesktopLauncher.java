package com.phoenixkahlo.hellcraft.desktop;

import com.badlogic.gdx.backends.lwjgl.LwjglApplication;
import com.badlogic.gdx.backends.lwjgl.LwjglApplicationConfiguration;
import com.phoenixkahlo.hellcraft.gamedriver.GameDriver;
import com.phoenixkahlo.hellcraft.menu.MainMenu;
import com.phoenixkahlo.hellcraft.singleplayer.FGraphicsCloudTest;

import java.awt.*;

public class DesktopLauncher {
	public static void main (String[] arg) {
		LwjglApplicationConfiguration config = new LwjglApplicationConfiguration();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		double factor = 0.18;
		config.width = (int) (screenSize.width * factor);
		config.height = (int) (screenSize.height * factor);
		config.title = "egress";
		new LwjglApplication(new GameDriver(new MainMenu()), config);
		//new LwjglApplication(new GameDriver(new FGraphicsCloudTest()), config);
		//new LwjglApplication(new CloudGenerator());
	}
}
