package pl.mradomski

import com.badlogic.gdx.graphics.g2d.{Sprite, SpriteBatch}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera, Texture}
import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.utils.GdxNativesLoader
import com.badlogic.gdx.utils.viewport.{ScreenViewport, Viewport}
import com.badlogic.gdx.{InputProcessor, Input, ApplicationAdapter, Gdx}

import scala.collection.mutable

class TimeAccumulator {
  var lastTimeMs = System.currentTimeMillis()
  var accumulatorSeconds = 0.0

  def reset() = {
    lastTimeMs = System.currentTimeMillis()
  }

  def update() = {
    val now = System.currentTimeMillis()
    accumulatorSeconds += (now - lastTimeMs) / 1000.0
    lastTimeMs = now
  }

  def tick(stepSeconds: Float) = {
    if (accumulatorSeconds > stepSeconds) {
      println(s"accumulator = $accumulatorSeconds")
      accumulatorSeconds -= stepSeconds
      true
    } else {
      false
    }
  }
}

class FluidApp extends ApplicationAdapter {
  private var batch: SpriteBatch = null
  private var img: Texture = null
  private var sprite: Sprite = null
  private var fluid: Fluid = null

  private var timeAccumulator = new TimeAccumulator()

  private var camera: OrthographicCamera = null
  private var viewport: Viewport = null

  override def create() {
    GdxNativesLoader.load()

    batch = new SpriteBatch
    img = new Texture("particle.png")
    sprite = new Sprite(img)
    sprite.setScale(0.2f, 0.2f)
    sprite.setColor(0.2f, 0.2f, 1.0f, 0.75f)
    sprite.setOriginCenter()

    camera = new OrthographicCamera()
    val yDown = false
    camera.setToOrtho(yDown)

    viewport = new ScreenViewport(camera)
    viewport.apply(true)
  }

  def getTouchPositions = {
    (0 to 8).filter(Gdx.input.isTouched).map {
      idx =>
        val ret = new Vector2(Gdx.input.getX(idx),
                              viewport.getScreenHeight - Gdx.input.getY(idx))
        println(s"touch @ ${ret.x} ${ret.y}")
        ret
    }
  }

  override def resize(width: Int, height: Int) {
    viewport.update(width, height, true)

    val numParticles: Int = 20
    val topLeft: Vector2 = new Vector2(0.0f, 0.0f)
    val bottomRight: Vector2 = new Vector2(width.toFloat, height.toFloat)
    fluid = new Fluid(numParticles, topLeft, bottomRight)

    timeAccumulator.reset()
  }

  override def render() = Utils.timeit("render") {
    val touchPositions = getTouchPositions

    timeAccumulator.update()
    val TIMESTEP = 0.1f
    while (timeAccumulator.tick(TIMESTEP)) {
      Utils.timeit("step") {
        fluid.step(TIMESTEP, touchPositions)
      }
    }

    Gdx.gl.glClearColor(0, 0, 0, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)

    camera.update()
    batch.setProjectionMatrix(camera.combined)
    batch.begin()
    fluid.draw(batch, sprite)
    batch.end()
  }
}