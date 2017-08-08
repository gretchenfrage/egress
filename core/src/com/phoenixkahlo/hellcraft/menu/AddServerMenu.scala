package com.phoenixkahlo.hellcraft.menu


import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.{Actor, InputEvent}
import com.badlogic.gdx.scenes.scene2d.ui.{Label, TextField}
import com.badlogic.gdx.scenes.scene2d.ui.Label.LabelStyle
import com.badlogic.gdx.scenes.scene2d.ui.TextField.TextFieldStyle
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.math.V2F
import com.phoenixkahlo.hellcraft.menu.util.{EButton, EButtonStyle, ETextField, ETextFieldStyle}
import com.phoenixkahlo.hellcraft.util.{Cache, HardDriveStringSet}
import other.AppDirs

class AddServerMenu(givenResources: Cache[ResourcePack]) extends AbstractMenu(givenResources) {

  override protected def escapeState = new ServerMenu(givenResources)

  override protected def compile(): Unit = {

    var y: Float = Gdx.graphics.getHeight / 2 + 200
    def position(widget: Actor): Unit = {
      widget.setPosition((Gdx.graphics.getWidth - widget.getWidth) / 2, y - widget.getHeight)
      y -= (widget.getHeight + 20)
      stage.addActor(widget)
    }

    val header = new Label("ADD SERVER", new LabelStyle(resources.font(TitleFID), Color.BLACK))
    position(header)

    val fieldStyle = ETextFieldStyle(
      resources.pixmap(MenuPatchPID),
      8,
      resources.font(ButtonFID),
      Color.BLACK,
      resources.sheetRegion(CursorTID),
      Math.min(Math.max(Gdx.graphics.getWidth * 0.4f, 400), Gdx.graphics.getWidth - 40).toInt,
      40,
      overshoot = V2F(5, 0)
    )
    val field = new ETextField(fieldStyle)
    position(field)

    val buttonStyle = EButtonStyle(
      resources.pixmap(MenuPatchPID),
      resources.pixmap(MenuPatchActivePID),
      8,
      resources.font(ButtonFID),
      Color.BLACK,
      Color.BLACK,
      100,
      35
    )
    val addButton = new EButton("ADD", buttonStyle)
    toDispose += addButton
    position(addButton)
    addButton.addListener(new ClickListener() {
      override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
        val text = field.getText
        val servers = HardDriveStringSet(AppDirs.dataDir("egress").resolve("servers.txt"))
        println("adding " + text + " to " + servers)
        servers.add(text)
        println("now is " + servers)
        driver.enter(new ServerMenu(new Cache(resources)))
      }
    })

  }

}
