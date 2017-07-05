package com.phoenixkahlo.hellcraft.desktop;

import com.badlogic.gdx.backends.lwjgl.LwjglApplication;
import com.badlogic.gdx.backends.lwjgl.LwjglApplicationConfiguration;
import com.phoenixkahlo.hellcraft.finitetest.SimpleDriver;
import com.phoenixkahlo.hellcraft.gamedriver.GameDriver;
import com.phoenixkahlo.hellcraft.infinitetest.InfiniteDriver;
import com.phoenixkahlo.hellcraft.infinitetest.InfiniteGameState;

public class DesktopLauncher {
	public static void main (String[] arg) {
		LwjglApplicationConfiguration config = new LwjglApplicationConfiguration();
		config.width = 1800;
		config.height = 800;
		//new LwjglApplication(new InfiniteDriver(), config);
		new LwjglApplication(new GameDriver(new InfiniteGameState()), config);
	}
}
