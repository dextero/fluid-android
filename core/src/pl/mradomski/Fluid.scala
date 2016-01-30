package pl.mradomski

import com.badlogic.gdx.graphics.g2d.{Sprite, SpriteBatch}
import com.badlogic.gdx.math.Vector2

object Constants {
  val GAS = 100.0f
  val GRAVITY = 9.81f
  val SUPPORT = 50.0f
}

object Assert {
  def validate(value: Float): Float = {
    if (value.isInfinite || value.isNaN) {
      throw new AssertionError()
    }
    value
  }

  def validate(value: Double): Double = {
    if (value.isInfinite || value.isNaN) {
      throw new AssertionError()
    }
    value
  }
}

import Assert._

object SmoothingKernel {
  class Spiky(support: Double) {
    val support6 = Math.pow(support, 6.0)

    def apply(distance: Double): Double = {
      if (0 <= distance && distance <= support) {
        val supp_r = support - distance
        val supp_r3 = supp_r * supp_r * supp_r
        validate(15.0 / (Math.PI * support6) * supp_r3)
      } else {
        0.0
      }
    }

    def gradient(pos: Vector2): Vector2 = {
      val r = pos.len

      if (0.0 < r && r <= support) {
        val supp_r = support - r
        val supp_r2 = supp_r * supp_r
        val scale = validate(-45.0 * supp_r2 / (support6 * Math.PI * r)).asInstanceOf[Float]
        new Vector2(pos.x * scale, pos.y * scale)
      } else {
        new Vector2()
      }
    }
  }

  class Poly6(support: Double) {
    val support2 = Math.pow(support, 2.0)
    val support9 = Math.pow(support, 9.0)

    def apply(distance2: Double): Double = {
      if (0 <= distance2 && distance2 <= support2) {
        val supp_r = support2 - distance2
        val supp_r3 = supp_r * supp_r * supp_r
        validate(315.0 / (64.0 * Math.PI * support9) * supp_r3)
      } else {
        0.0
      }
    }
  }

  class Viscosity(support: Double) {
    val support6 = Math.pow(support, 6.0)

    def laplacian(pos: Vector2): Double = {
      val len = pos.len
      if (0 <= len && len <= support) {
        45.0 / (Math.PI * support6) * (support - len)
      } else {
        0.0
      }
    }
  }
}

object Particle {
  private def randomFloat(min: Float, max: Float): Float = {
    Math.random().asInstanceOf[Float] * (max - min) + min
  }

  def random(topLeft: Vector2,
             bottomRight: Vector2): Particle = {
    val pos = new Vector2(randomFloat(topLeft.x, bottomRight.x),
                          randomFloat(topLeft.y, bottomRight.y))
    val v = new Vector2(randomFloat(-1.0f, 1.0f),
                        randomFloat(-1.0f, 1.0f))
    val mass = 1.0f
    Particle(pos, v, mass)
  }
}

case class Particle(pos: Vector2,
                    velocity: Vector2,
                    mass: Float) {
  def updated(fluid: Fluid,
              dt: Float,
              topLeft: Vector2,
              bottomRight: Vector2): Particle = {
    val p = velocity.scl(dt).add(pos)
    val v = fluid.acceleration(pos, velocity).scl(dt)
    v.y -= Constants.GRAVITY * dt; // gravity

    if (p.x < topLeft.x) {
      p.x = topLeft.x
      v.x = -v.x
    } else if (p.x > bottomRight.x) {
      p.x = bottomRight.x
      v.x = -v.x
    }

    if (p.y < topLeft.y) {
      p.y = topLeft.y
      v.y = -v.y
    } else if (p.y > bottomRight.y) {
      p.y = bottomRight.y
      v.y = -v.y
    }

    Particle(pos = p,
             velocity = v,
             mass = mass)
  }
}

case class Fluid(numParticles: Int,
                 topLeft: Vector2,
                 bottomRight: Vector2) {
  val REST_DENSITY = 0.0f
  val VISCOSITY = 1.0f

  val support = Constants.SUPPORT
  val poly6Kernel = new SmoothingKernel.Poly6(support)
  val spikyKernel = new SmoothingKernel.Spiky(support)
  val viscosityKernel = new SmoothingKernel.Viscosity(support)
  var particles = Array.fill[Particle](numParticles)(Particle.random(topLeft, bottomRight))

  def density(pos: Vector2): Double = {
    particles.fold(0.0) {
      case (sum: Double, particle: Particle) =>
        validate(sum + particle.mass * poly6Kernel(particle.pos.dst2(pos)))
    }.asInstanceOf[Double]
  }

  def pressure(pos: Vector2) = Constants.GAS * (density(pos) - REST_DENSITY)

  def forceDensity(pos: Vector2): Vector2 = {
    val negated = particles.foldLeft(new Vector2()) {
      case (sum: Vector2, particle: Particle) =>
        val dir = pos.cpy.sub(particle.pos)
        val scale = (pressure(pos) + pressure(particle.pos)) / (2.0 * density(particle.pos))
        val result = sum.add(spikyKernel.gradient(dir).scl(scale.asInstanceOf[Float]))
        validate(result.x)
        validate(result.y)
        result
    }

    new Vector2().sub(negated)
  }

  def forceViscosity(pos: Vector2,
                     velocity: Vector2): Vector2 = {
    val force = particles.foldLeft(new Vector2()) {
      case (sum: Vector2, particle: Particle) =>
        val dv = particle.velocity.cpy.sub(velocity)
        dv.scl((1.0 / density(particle.pos)).asInstanceOf[Float])
        val kernelFactor = viscosityKernel.laplacian(pos.cpy.sub(particle.pos))
        val result = sum.mulAdd(dv, (particle.mass * kernelFactor).asInstanceOf[Float])
        validate(result.x)
        validate(result.y)
        result
    }

    force.scl(VISCOSITY)
  }

  def acceleration(pos: Vector2, velocity: Vector2): Vector2 = {
    forceDensity(pos).scl(1.0f / density(pos).asInstanceOf[Float])
      .add(forceViscosity(pos, velocity))
  }

  def step(dt_unscaled: Float): Unit = {
    val dt = dt_unscaled * 10.0f
    particles = particles.map {
      particle => particle.updated(this, dt, topLeft, bottomRight)
    }
  }

  def draw(batch: SpriteBatch,
           sprite: Sprite) = {
    particles.foreach {
      p =>
        sprite.setPosition(p.pos.x, p.pos.y)
        sprite.draw(batch)
    }
  }
}
