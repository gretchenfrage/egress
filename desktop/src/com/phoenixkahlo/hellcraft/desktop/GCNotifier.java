package com.phoenixkahlo.hellcraft.desktop;

public class GCNotifier {
    @Override
    protected void finalize() throws Throwable {
        System.out.println("GCing");
        new GCNotifier();
    }
}
