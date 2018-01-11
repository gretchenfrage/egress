package com.phoenixkahlo.hellcraft.desktop;

import com.badlogic.gdx.backends.lwjgl.LwjglApplication;
import com.badlogic.gdx.backends.lwjgl.LwjglApplicationConfiguration;
import com.phoenixkahlo.hellcraft.bullet.BulletTest;
import com.phoenixkahlo.hellcraft.gamedriver.GameDriver;
import com.phoenixkahlo.hellcraft.menu.MainMenu;

import java.awt.*;

public class DesktopLauncher {
	public static void main (String[] arg) {
		//new GCNotifier();
		LwjglApplicationConfiguration config = new LwjglApplicationConfiguration();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		double factor = 0.18;
		config.width = (int) (screenSize.width * factor);
		config.height = (int) (screenSize.height * factor);
		config.title = "Egress";
		//config.foregroundFPS = 1000;
		//config.vSyncEnabled = false;

		new LwjglApplication(new GameDriver(new MainMenu()), config);
		//new LwjglApplication(new GameDriver(new BulletTest()), config);

		//new LwjglApplication(new GameDriver(new FGraphicsCloudTest()), config);
		//new LwjglApplication(new CloudGenerator());
	}
}
