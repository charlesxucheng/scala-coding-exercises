package misc

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest._

trait UnitSpec extends AnyWordSpec with Matchers with OptionValues with Inside with Inspectors with BeforeAndAfterAll with BeforeAndAfterEach
