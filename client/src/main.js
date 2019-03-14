// Viktigt att tänka på dock är att när man importerar en fil så skapas en instans av den filen. Exempel: Säg att vi importerar en fil här: "import './router.js'"
// Importerar man './router.js' från ett annat ställe i projektet så kommer man då få SAMMMA INSTANS av router.
//
// Man kan importa båda filer i projektet och installerade packet (npm install X). Första raden är ett installerat packet medans andra raden är en fil i projektet. 
// (Notera 'vue' vs './App.vue') 
import Vue from 'vue';
import App from './App.vue';
import router from './router';
import store from './store/';
import axios from 'axios'
import BootstrapVue from 'bootstrap-vue'
import 'bootstrap/dist/css/bootstrap.css'
import 'bootstrap-vue/dist/bootstrap-vue.css'

Vue.use(BootstrapVue);	// Lägg till stöd för bootstrap i vår app.

// När man har ett objekt X och gör X.prototype på det så ändrar man hur varje objekt X som skapas ser ut.
// Här lägger vi till en variabel $http i Vue-prototypen.
// Detta innebär att när vi gör 'new Vue(...)' lite längre ner så kommer denna instansen av Vue att ha en variabel ($http) som är en axios instans (axios sköter http requests)
Vue.prototype.$http = axios.create({
	baseURL: 'http://localhost:8080/api',
	timeout: 5000
});;

Vue.config.productionTip = false;

new Vue({	// Alla saker som vi ger in här agerar som plugins i vår Vue app. Dessa plugins ändrar hur Vue fungerar
	router,	// Lägg till router-plugin. Detta gör att vi kan gå mellan olika sidor utan att behöva ladda om bland annat
	store,	// Lägg till store-plugin. Denna plugin gör att vi kan ha en gemensam state i hela vår app. Man skulle kunna säga att store blir som ett globalt objekt.
	render: h => h(App)	// Säg till Vue vilken .vue-fil som ska användas som root på hemsidan. I detta fall App.vue
}).$mount('#app');	// Sätt fast på ett html objektet med id=app
