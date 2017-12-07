package com.phoenixkahlo.hellcraft.fgraphics

import com.badlogic.gdx.graphics.Camera
import com.phoenixkahlo.hellcraft.math.V3F

case class GlobalRenderData(camPos: V3F, camDir: V3F, lightPos: V3F, lightPow: Float, cam: Camera)
