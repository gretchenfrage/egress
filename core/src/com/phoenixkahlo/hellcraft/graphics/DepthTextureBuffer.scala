package com.phoenixkahlo.hellcraft.graphics

/*
import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.{GL20, Pixmap, Texture, TextureData}

class DepthTextureBuffer(size: Int) {

  val framebufferName: Int = Gdx.gl.glGenFramebuffer()
  Gdx.gl.glBindFramebuffer(1, framebufferName)

  val depthTexture: Int = Gdx.gl.glGenTexture()
  Gdx.gl.glBindTexture(GL20.GL_TEXTURE_2D, depthTexture)
  Gdx.gl.glTexImage2D(GL20.GL_TEXTURE_2D, 0, GL20.GL_DEPTH_COMPONENT16, size, size, 0, GL20.GL_DEPTH_COMPONENT, GL20.GL_FLOAT, null)
  Gdx.gl.glTexParameteri(GL20.GL_TEXTURE_2D, GL20.GL_TEXTURE_MAG_FILTER, GL20.GL_NEAREST)
  Gdx.gl.glTexParameteri(GL20.GL_TEXTURE_2D, GL20.GL_TEXTURE_MIN_FILTER, GL20.GL_NEAREST)
  Gdx.gl.glTexParameteri(GL20.GL_TEXTURE_2D, GL20.GL_TEXTURE_WRAP_S, GL20.GL_CLAMP_TO_EDGE)
  Gdx.gl.glTexParameteri(GL20.GL_TEXTURE_2D, GL20.GL_TEXTURE_WRAP_T, GL20.GL_CLAMP_TO_EDGE)

  Gdx.gl.glFramebufferTexture2D(GL20.GL_FRAMEBUFFER, GL20.GL_DEPTH_ATTACHMENT, depthTexture, 0)



}

*/