<template>
	<div class="nav primary-color">
		<div class="logo-container">
			<router-link id="home" to="/" tag="div">
				<img src="@/assets/logo.png" />
			</router-link>
		</div>
		<NavButton text="Admin" to="/admin"></NavButton>
	</div>
</template>

<script>
import NavButton from '@/components/Navigation/NavButton.vue';
export default {
	name: 'Navigation',
	components: {
		NavButton
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.nav {
	width: 100%;
	display: flex;
	align-items: center;
}
#home {
	height: 100%;
	display: flex;
	justify-content: center; /* align horizontal */
	align-items: center; /* align vertical */
}
#home img {
	max-width: 60%;
	max-height: 60%;
}
.logo-container {
	width: 100px;
	height: 50px;
	-webkit-transition-duration: 0.4s; /* Safari */
	transition-duration: 0.4s;
}
.logo-container:hover {
	color: white;
	background-color: #2c3e50;
	filter: brightness(85%);
	cursor: default;
}
.logo-container div {
	height: 100%;
	display: flex;
	justify-content: center; /* align horizontal */
	align-items: center; /* align vertical */
}
</style>
