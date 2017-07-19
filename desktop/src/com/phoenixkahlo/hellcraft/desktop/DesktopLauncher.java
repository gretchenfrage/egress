package com.phoenixkahlo.hellcraft.desktop;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Graphics;
import com.badlogic.gdx.backends.lwjgl.LwjglApplication;
import com.badlogic.gdx.backends.lwjgl.LwjglApplicationConfiguration;
import com.esotericsoftware.minlog.Log;
import com.phoenixkahlo.hellcraft.finitetest.SimpleDriver;
import com.phoenixkahlo.hellcraft.gamedriver.GameDriver;
import com.phoenixkahlo.hellcraft.gamedriver.LoopExecutor;
import com.phoenixkahlo.hellcraft.infinitetest.InfiniteDriver;
import com.phoenixkahlo.hellcraft.infinitetest.InfiniteGameState;
import com.phoenixkahlo.hellcraft.multiplayertest.GameClient;
import com.phoenixkahlo.hellcraft.multiplayertest.GameServer;

import java.awt.*;
import java.io.File;
import java.net.InetSocketAddress;
import java.util.Arrays;

public class DesktopLauncher {
	public static void main (String[] arg) {
		LwjglApplicationConfiguration config = new LwjglApplicationConfiguration();
		//config.width = 1800;
		//config.height = 800;
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		double factor = 0.5;
		config.width = (int) (screenSize.width * factor);
		config.height = (int) (screenSize.height * factor);

		/*
		File mul = new File("C:\\Users\\kahlo\\Desktop\\mul");
		if (mul.exists()) Arrays.asList(mul.listFiles()).forEach(File::delete);
		new LoopExecutor(new GameServer(25565)).start();
		try { Thread.sleep(5000); } catch (Exception e) { throw new RuntimeException(e); }
		new LwjglApplication(new GameDriver(new GameClient(new InetSocketAddress("localhost", 25565))), config);
		*/

		new LwjglApplication(new GameDriver(new InfiniteGameState()), config);
		//new LwjglApplication(new InfiniteDriver(), config);
		//new LwjglApplication(new SimpleDriver(), config);
	}
}
