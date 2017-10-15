package test.jackson

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import libs.Libs._
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.SerializationFeature
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.databind.DeserializationFeature
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.databind.PropertyNamingStrategy
import org.scalatest.FlatSpec


object Castles04 {
  
  sealed trait Castle extends HasIntValue
  case object edo extends Castle
  {
    override val value:Int = 1
  }
  case object osaka extends Castle
  {
    override val value:Int = 2
  }
  case object azuchi extends Castle
  {
    override val value:Int = 3
  }
  case object other extends Castle
  {
    override val value:Int = 99
  }
}



object TestJackson04
{

  import Castles04._
  
  case class Parent01(name:String,militaryCommanders:Seq[MilitaryCommander01])
  
  //@JsonIgnoreProperties(ignoreUnknown=true)
  case class MilitaryCommander01(name:Option[String],age:Option[Int]
  ,castle:Castle
  )

  case class Parent02(name:String,militaryCommanders:Seq[MilitaryCommander02])
  
  //@JsonIgnoreProperties(ignoreUnknown=true)
  case class MilitaryCommander02(name:Option[String],age:Option[Int]
  ,castle:Option[Castle]
  )
  
  
}

class TestJackson04 extends FlatSpec with MustMatchers{
  
  import TestJackson04._
    
  
    import Castles04._
    
    val clazz = classOf[Castle]
    val `type` = scala.reflect.runtime.universe.typeOf[Castles04.type]
    
    val customModule = new SimpleModule("CustomSerializer")
    .addSerializer(clazz, new CaseObjectSerializerWithValue)
    .addDeserializer(clazz, new CaseObjectDeserializerWithValue(clazz,classOf[Int],`type`))
          
    val mapper=new ObjectMapper()
      .enable(SerializationFeature.INDENT_OUTPUT)
      .setSerializationInclusion(JsonInclude.Include.NON_NULL)
      .setSerializationInclusion(JsonInclude.Include.NON_ABSENT)
      .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
      .registerModule(customModule)
      .registerModule(DefaultScalaModule)
      
    val Code1 = "code4の1"  
    val Code2 = "code4の2"  
    it should s"${Code1} Castleを非Option型で" in
    {
      val source = Parent01(name=s"リスト${Code1}",militaryCommanders=Seq(
        MilitaryCommander01(name=Some("秀吉"),age=Some(62),castle=osaka)    
        ,MilitaryCommander01(name=Some("家康"),age=Some(76),castle=edo)    
        ,MilitaryCommander01(name=Some("信長"),age=None,castle=azuchi)    
        ,MilitaryCommander01(name=Some("足軽その１"),age=None,castle=other)    
      )) 
      
      val json1 = mapper.writeValueAsString(source)
      println(s"${Code1}-json:${json1}")
      val result = mapper.readValue(json1, classOf[Parent01])
      println(s"${Code1}-instance:${result}")
      val json2 = mapper.writeValueAsString(result)
      println(s"${Code1}-json:${json2}")
      
      result.militaryCommanders(0).castle must ===(osaka)
      result.militaryCommanders(1).castle must ===(edo)
      result.militaryCommanders(2).castle must ===(azuchi)
      result.militaryCommanders(3).castle must ===(other)
      
      json1 must ===(json2)
      
    }
    
    it should s"${Code2} CastleをOption型で" in
    {
      val source = Parent02(name=s"リスト${Code2}",militaryCommanders=Seq(
        MilitaryCommander02(name=Some("秀吉"),age=Some(62),castle=Some(osaka))    
        ,MilitaryCommander02(name=Some("家康"),age=Some(76),castle=Some(edo))    
        ,MilitaryCommander02(name=Some("信長"),age=None,castle=Some(azuchi))    
        ,MilitaryCommander02(name=Some("足軽その１"),age=None,castle=None)    
      )) 
      
      val json1 = mapper.writeValueAsString(source)
      println(s"${Code2}-json:${json1}")
      val result = mapper.readValue(json1, classOf[Parent02])
      println(s"${Code2}-instance:${result}")
      val json2 = mapper.writeValueAsString(result)
      println(s"${Code2}-json:${json2}")
      
      result.militaryCommanders(0).castle.get must ===(osaka)
      result.militaryCommanders(1).castle.get must ===(edo)
      result.militaryCommanders(2).castle.get must ===(azuchi)
      result.militaryCommanders(3).castle must ===(None)
      
      json1 must ===(json2)
      
    }
    
 
  
  
}