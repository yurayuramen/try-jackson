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


object Castles03 {

  sealed trait Castle extends HasStringValue
  case object edo extends Castle
  {
    override val value = "江戸"
  }
  case object osaka extends Castle
  {
    override val value = "大阪"
  }
  case object azuchi extends Castle
  {
    override val value = "安土"
  }
  case object other extends Castle
  {
    override val value = "その他"
  }
}




object TestJackson03YAML
{

  
  
  case class Parent01(name:String,militaryCommanders:Seq[MilitaryCommander01])
  
  //@JsonIgnoreProperties(ignoreUnknown=true)
  case class MilitaryCommander01(name:Option[String],age:Option[Int]
  ,castle:Castles03.Castle
  )

  case class Parent02(name:String,militaryCommanders:Seq[MilitaryCommander02])
  
  //@JsonIgnoreProperties(ignoreUnknown=true)
  case class MilitaryCommander02(name:Option[String],age:Option[Int]
  ,castle:Option[Castles03.Castle]
  )
  

  
  
}

class TestJackson03YAML extends FlatSpec with MustMatchers{
  
  import TestJackson03YAML._
    
  import Castles03._
  
  val clazz = classOf[Castle]
  val `type` = scala.reflect.runtime.universe.typeOf[Castles03.type]
  
  val customModule = new SimpleModule("CustomSerializer")
  .addSerializer(clazz, new CaseObjectSerializerWithValue)
  .addDeserializer(clazz, new CaseObjectDeserializerWithValue(clazz,classOf[String],`type`))
        
  val mapper=new ObjectMapper(new YAMLFactory)
    .enable(SerializationFeature.INDENT_OUTPUT)
    .setSerializationInclusion(JsonInclude.Include.NON_NULL)
    //
    // https://github.com/fasterxml/jackson-databind/issues/811
    //
    .setSerializationInclusion(JsonInclude.Include.NON_ABSENT)
    //
    // https://stackoverflow.com/questions/5455014/ignoring-new-fields-on-json-objects-using-jackson
    //
    .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
    //.setPropertyNamingStrategy(PropertyNamingStrategy.CAMEL_CASE_TO_LOWER_CASE_WITH_UNDERSCORES)
    .registerModule(customModule)
    .registerModule(DefaultScalaModule)
  
      
    val Code1 = "code3の1"  
    val Code2 = "code3の2"  
    val Code3 = "code3の3"  
    val Code4 = "code3の4"  
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
      result.militaryCommanders(2).castle must ===(Some(azuchi))
      result.militaryCommanders(3).castle must ===(None)
      
      json1 must ===(json2)
      
    }
    it should s"${Code3} Castleを非Option型で" in
    {
      
      val source = Map("name"->s"リスト${Code3}"
        ,"militaryCommanders"->Seq(
        Map("name"->"秀吉","age"->"3","castle"->"大阪")
        ,Map("name"->"信長","castle"->"江戸")
        ,Map("age"->49,"castle"->"清州")
        )) 

      
      val json1 = mapper.writeValueAsString(source)
      println(s"${Code3}-json:${json1}")
      val result = mapper.readValue(json1, classOf[Parent01])
      println(s"${Code3}-instance:${result}")
      val json2 = mapper.writeValueAsString(result)
      println(s"${Code3}-json:${json2}")
      
      result.militaryCommanders(0).castle must ===(osaka)
      result.militaryCommanders(1).castle must ===(edo)
      //result.militaryCommanders(2).castle must ===(azuchi)
      result.militaryCommanders(2).castle must ===(null)
      
      json1 must !==(json2)
      
    }
    
    it should s"${Code4} CastleをOption型で" in
    {
      
      val source = Map("name"->s"リスト${Code4}"
        ,"militaryCommanders"->Seq(
        Map("name"->"秀吉","age"->"3","castle"->"大阪")
        ,Map("name"->"信長","castle"->"江戸")
        ,Map("age"->49 ,"castle"->"清州")
        ) )

      
      val json1 = mapper.writeValueAsString(source)
      println(s"${Code4}-json:${json1}")
      val result = mapper.readValue(json1, classOf[Parent02])
      println(s"${Code4}-instance:${result}")
      val json2 = mapper.writeValueAsString(result)
      println(s"${Code4}-json:${json2}")
      
      result.militaryCommanders(0).castle.get must ===(osaka)
      result.militaryCommanders(1).castle.get must ===(edo)
      //result.militaryCommanders(2).castle must ===(azuchi)
      result.militaryCommanders(2).castle must ===(None)
      
      json1 must !==(json2)
      
    }
    
}