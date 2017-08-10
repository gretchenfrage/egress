package com.phoenixkahlo.hellcraft.desktop;

import com.badlogic.gdx.backends.lwjgl.LwjglApplication;
import com.badlogic.gdx.backends.lwjgl.LwjglApplicationConfiguration;
import com.esotericsoftware.minlog.Log;
import com.phoenixkahlo.hellcraft.graphics.DefaultResourcePack;
import com.phoenixkahlo.hellcraft.graphics.ResourcePack;
import com.phoenixkahlo.hellcraft.gamedriver.GameDriver;
import com.phoenixkahlo.hellcraft.menu.MainMenu;
import com.phoenixkahlo.hellcraft.multiplayertest.ClientServerState;
import com.phoenixkahlo.hellcraft.util.Cache;

import java.awt.*;

public class DesktopLauncher {
	public static void main (String[] arg) {
		LwjglApplicationConfiguration config = new LwjglApplicationConfiguration();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		double factor = 0.5;
		config.width = (int) (screenSize.width * factor);
		config.height = (int) (screenSize.height * factor);

		/*
		Path dir = AppDirs.dataDir("egress");
		System.out.println("using directory " + dir);
		dir.toFile().mkdir();
		File mul = AppDirs.dataDir("egress").resolve("mul").toFile();
		if (mul.exists()) Arrays.asList(mul.listFiles()).forEach(File::delete);
		new Thread(new EgressServer(25565)).start();
		try { Thread.sleep(5000); } catch (Exception e) { throw new RuntimeException(e); }
		InetSocketAddress address = new InetSocketAddress("localhost", 25565);
		Cache<TexturePack> textures = new Cache<>(DefaultTexturePack::new);
		new LwjglApplication(new GameDriver(new EgressClient(address, textures)), config);
		*/
		//Cache<ResourcePack> resources = new Cache<>(DefaultResourcePack::new);
		//new LwjglApplication(new GameDriver(new ClientServerState(resources)), config);
		new LwjglApplication(new GameDriver(new MainMenu()), config);

		//InterframeChunkCompressionTest.test();

		//new LwjglApplication(new GameDriver(new InfiniteGameState()), config);
		//new LwjglApplication(new InfiniteDriver(), config);
		//new LwjglApplication(new SimpleDriver(), config);
	}
}
