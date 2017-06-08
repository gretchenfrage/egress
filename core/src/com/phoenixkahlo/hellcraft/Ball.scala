package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g3d.attributes.{ColorAttribute, FloatAttribute, TextureAttribute}
import com.badlogic.gdx.graphics.g3d.{Material, ModelInstance}
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.graphics.{Color, Texture}
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.math.{Matrix4, Vector3}
import com.badlogic.gdx.physics.bullet.collision.btSphereShape
import com.badlogic.gdx.physics.bullet.dynamics.btRigidBody
import com.badlogic.gdx.physics.bullet.linearmath.btMotionState
import com.phoenixkahlo.hellcraft.util.V3F

class BallMotionState(transform: Matrix4) extends btMotionState {
  override def getWorldTransform(worldTrans: Matrix4): Unit =
    worldTrans.set(transform)

  override def setWorldTransform(worldTrans: Matrix4): Unit =
    transform.set(worldTrans)
}

class Ball(
          val radius: Float
          ) {

  val model: ModelInstance = {
    val texture = new Texture(Gdx.files.internal("sand.png"))
    val builder = new ModelBuilder()
    val m = builder.createSphere(
      radius * 2, radius * 2, radius * 2, 24, 24,
      new Material(
        TextureAttribute.createDiffuse(texture),
        ColorAttribute.createSpecular(1, 1, 1, 1),
        FloatAttribute.createShininess(8f)
      ),
      Usage.Position | Usage.Normal | Usage.TextureCoordinates
    )
    new ModelInstance(m)
  }
  val constructionInfo: btRigidBody.btRigidBodyConstructionInfo = {
    val shape = new btSphereShape(radius)
    val mass = (4f / 3f * Math.PI * Math.pow(radius, 3) * 1.259f).toFloat
    val localInertia = new Vector3()
    if (mass > 0)
      shape.calculateLocalInertia(mass, localInertia)
    new btRigidBody.btRigidBodyConstructionInfo(mass, null, shape, localInertia)
  }
  val body: btRigidBody = new btRigidBody(constructionInfo)
  val motionState = new BallMotionState(model.transform)
  body.setMotionState(motionState)


}
